extern crate num;
use ::std::collections::HashMap;
use anyhow::{bail, Result};
use crossterm::terminal::{Clear, EnterAlternateScreen, LeaveAlternateScreen};
use crossterm::{execute, terminal, ExecutableCommand};
use num::bigint::BigInt;
use num::{
    integer::Roots,
    traits::{FloatConst, One, Zero},
    Float, FromPrimitive, Num, NumCast,
};
use rustyline::{config::Configurer, DefaultEditor};
use std::{
    fmt::Debug,
    fs::File,
    io::{stdout, BufRead, BufReader, BufWriter, Write},
    path::PathBuf,
};

const HISTORY_SIZE: u8 = 8;

fn main() -> Result<()> {
    let mut app = RpnCalculator::new();
    app_execute(&mut app)?;
    Ok(())
}

fn app_execute(app: &mut RpnCalculator<f64>) -> Result<()> {
    let mut my_readline = DefaultEditor::new()?;
    my_readline.set_max_history_size(20)?;

    let mut stdout = stdout();
    stdout.execute(EnterAlternateScreen)?;

    writeln!(
        stdout,
        "RPN Calculator.\nInput 'help' to see functions.\nPress Enter key."
    )?;
    writeln!(stdout, "{}", "\n".repeat(14),)?;
    stdout.flush()?;

    loop {
        let is_continue = manage_token(app, &mut my_readline)?;
        if !is_continue && is_sure("Will you quit rpncalc?")? {
            break;
        }
        execute!(&stdout, Clear(terminal::ClearType::All))?;

        app.last_stack_length = app.stack_vec.stack.len();

        // 変数ストックと関数ストックの表示用文字列の準備
        let mut memos = String::new();
        let mut funcmemo = String::new();

        // 変数ストックにアイテムが存在する時、表示用文字列に追加
        if !app.mem_value.is_empty() {
            app.mem_value.keys().for_each(|mem_key| {
                memos += &format!("{}\u{22B3}{:},", mem_key, app.mem_value[mem_key]);
            });
        }

        // 関数ストックにアイテムが存在する時、表示用文字列に追加
        if !app.func_mem.is_empty() {
            app.func_mem.keys().enumerate().for_each(|(i, func_key)| {
                funcmemo += &format!("{} = {},  ", func_key, app.func_mem[func_key]);
                if (i + 1) % 5 == 0 {
                    funcmemo += "\n";
                }
            });
        }

        writeln!(
            &mut stdout,
            "Memo: {}\nFunc: {}\n[{:#?}] [{:?}] [{:?}:{}] [StackLen:{}] [UndoLen:{}]",
            memos,
            funcmemo,
            app.angle_mode,
            app.base_num,
            app.display_format,
            app.disp_pointnum,
            app.stack_vec.stack.len(),
            app.stack_vec.undo_stack.len(),
        )?;

        let max_disp_stack_len = 8;
        let stacklen = app.stack_vec.stack.len();
        let last_stack_len = app.last_stack_length;

        let display_stack_length = if last_stack_len > max_disp_stack_len {
            last_stack_len - max_disp_stack_len
        } else {
            0
        };

        if stacklen > 2 && app.base_num == BaseNum::Dec {
            for (i, s) in app.stack_vec.stack[display_stack_length..last_stack_len - 2]
                .iter()
                .enumerate()
            {
                writeln!(
                    &mut stdout,
                    "[{}]: {:<20}",
                    last_stack_len - display_stack_length - i,
                    s
                )?;
            }
        }

        // 最後から２番目の値
        if stacklen > 1 {
            let lnum = &app.stack_vec.stack[stacklen - 2];
            let disp_num = convnum(app, lnum);
            writeln!(&mut stdout, "[2]: {:<20}", disp_num,)?;
        }

        write!(&mut stdout, "{:<25}", "\u{2500}".repeat(22),)?;
        writeln!(
            &mut stdout,
            "History >>{:<20}",
            &app.token_history.join(" ")
        )?;

        // 警告メッセージ
        if !app.remark_message.is_empty() {
            write!(&mut stdout, "{:<25}", "\u{2500}".repeat(22),)?;
            writeln!(&mut stdout, "{:<20}", app.remark_message)?;
        }

        // 最後の値
        app.result_message.truncate(50);

        if let Some(lastnum) = app.stack_vec.stack.last() {
            let disp_num = convnum(app, lastnum);
            writeln!(&mut stdout, "[1]: {:<20}{}", disp_num, &app.result_message)?;
        } else {
            writeln!(&mut stdout, "     {:<20}{}", "", &app.result_message)?;
        }

        stdout.flush()?;
    }

    stdout.execute(LeaveAlternateScreen)?;
    Ok(())
}

///入力されたトークンをマネジメントする
/// q もしくは　quitでResult<false>を返す
fn manage_token(app: &mut RpnCalculator<f64>, my_readline: &mut DefaultEditor) -> Result<bool> {
    let in_token: String;

    match app.read_token.pop() {
        // 外部ファイルを読み込んだとき (readを使用)
        Some(readtoken) => {
            in_token = readtoken;
        }
        None => {
            // 外部ファイルを使用しないとき,
            // 標準入力から読み込んで処理
            let readline = my_readline.readline(">> ");

            match readline {
                Ok(token) => {
                    in_token = token.trim().to_lowercase();
                    if &in_token == "q" || &in_token == "quit" {
                        return Ok(false);
                    }
                }
                Err(_) => {
                    in_token = "".to_string();
                }
            }

            app.result_message.clear();
            app.remark_message.clear();

            match app.stack_vec.undo_stack.is_empty()
                || Some(&app.stack_vec.stack) != app.stack_vec.undo_stack.last()
            {
                true => {
                    app.stack_vec.backup();
                }
                false => (),
            }
        }
    }

    // 計算結果にエラーがなければ計算結果
    match app.calc(&in_token) {
        Err(e) => {
            let errormsg = e.to_string();
            app.add_result_message(&errormsg);
        }
        Ok(_) => {
            app.add_result_message(&("<= ".to_string() + &in_token));
            my_readline.add_history_entry(in_token)?;
        }
    }
    Ok(true)
}

fn readfile(path: &str) -> Result<Vec<String>> {
    let fpath = PathBuf::from(path);
    let fs = File::open(fpath)?;
    let reader = BufReader::new(fs);
    let mut readstrings = Vec::new();

    for line in reader.lines() {
        let ordertext = line?;
        if ordertext.contains('#') {
        } else {
            readstrings.push(ordertext);
        }
    }
    readstrings.reverse();

    Ok(readstrings)
}

#[derive(Debug, PartialEq, Clone)]
enum DispFormat {
    Fix,
    Eng,
    All,
}

#[derive(Debug, Clone, PartialEq)]
enum BaseNum {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, Clone)]
enum AngleMode {
    Rad,
    Deg,
}

#[derive(Debug, Clone)]
struct StackVec<T: Num + Clone> {
    stack: Vec<T>,
    undo_stack: Vec<Vec<T>>,
}

impl<T: Num + Clone + Debug + num::FromPrimitive> StackVec<T> {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            undo_stack: Vec::new(),
        }
    }

    fn pop(&mut self) -> Option<T> {
        self.stack.pop()
    }

    fn push(&mut self, item: T) {
        self.stack.push(item);
    }

    fn clear(&mut self) {
        self.stack.clear();
    }

    fn len(&self) -> T {
        num::FromPrimitive::from_usize(self.stack.len()).unwrap()
    }
    fn backup(&mut self) {
        // undo Vecへスタックを追加
        self.undo_stack.push(self.stack.to_owned());
        // undo_stackを一定サイズにトリミング
        self.history_truncate();
    }

    fn undo(&mut self) -> Result<()> {
        // 最新のundo_stackはstackと同じなので捨てる。
        self.undo_stack.pop();
        // undo_stackが残っていればundo_stackのlastを表示stackとする。
        let prestack = self.undo_stack.pop();
        if let Some(prestack) = prestack {
            self.stack = prestack;
        }
        Ok(())
    }

    fn history_truncate(&mut self) {
        if self.undo_stack.len() > HISTORY_SIZE.into() {
            self.undo_stack.remove(0);
        }
    }
}

#[derive(Debug, Clone)]
struct RpnCalculator<T: Num + Clone> {
    stack_vec: StackVec<T>,
    single_functions: Vec<String>,
    no_pop_func: Vec<String>,
    another_func: Vec<String>,
    angle_mode: AngleMode,
    base_num: BaseNum,
    mem_value: HashMap<String, T>,
    func_mem: HashMap<String, String>,
    last_stack_length: usize,
    token_history: Vec<String>,
    result_message: String,
    remark_message: String,
    read_token: Vec<String>,
    display_format: DispFormat,
    disp_pointnum: usize,
}

impl<T: Float + FloatConst + FromPrimitive + Debug> RpnCalculator<T> {
    fn new() -> Self {
        let mut single_funcs = str_to_vec(
            "sin cos tan asin acos atan del dd sqrt sq torad todeg abs round 1/x -x frac ! log",
        );
        let mut no_pop_funcs = str_to_vec(
            "clr cls c pi e sum avg rad deg swap sw mclr mcls fclr fcls def fn allcls allclr bin oct dec hex undo u primes prs",
        );
        let mut another_funcs = str_to_vec(
            "logx pow mod div sto fdel mdel to nrt rp repeat max min roll and or xor >> << dsfix dseng dsall ncr npr gcm lcm",
        );

        single_funcs.sort();
        no_pop_funcs.sort();
        another_funcs.sort();

        Self {
            stack_vec: StackVec::new(),
            single_functions: single_funcs,
            no_pop_func: no_pop_funcs,
            another_func: another_funcs,
            angle_mode: AngleMode::Deg,
            base_num: BaseNum::Dec,
            mem_value: HashMap::with_capacity(16),
            func_mem: HashMap::with_capacity(16),
            last_stack_length: 0,
            token_history: Vec::with_capacity(16),
            result_message: String::with_capacity(16),
            remark_message: String::with_capacity(16),
            read_token: Vec::with_capacity(16),
            display_format: DispFormat::Fix,
            disp_pointnum: 3,
        }
    }

    fn add_result_message(&mut self, message: &str) {
        let base_messege = self.result_message.to_owned();
        self.result_message = base_messege + message + " ";
    }

    fn is_ok_mem_string(&mut self, token: &str, isfunc: bool) -> bool {
        if isfunc {
            if self.mem_value.contains_key(token) {
                return false;
            };
        } else if self.func_mem.contains_key(token) {
            return false;
        };

        match token.parse::<f64>() {
            Ok(_) => return false,
            Err(_) => {
                let functions = [
                    &self.another_func,
                    &self.single_functions,
                    &self.no_pop_func,
                ];

                for funcs in functions.iter() {
                    if funcs.contains(&token.to_string()) {
                        return false;
                    }
                }
            }
        };
        true
    }

    ///計算処理の前に空や終了コマンドを判別し計算処理に移る
    fn calc(&mut self, in_token: &str) -> Result<()> {
        // トークンを空白で区切ってVecに格納
        let tokens = in_token.split_whitespace().rev().collect::<Vec<_>>();

        // トークンが空か確認
        if tokens.is_empty() {
            let item = match self.stack_vec.stack.last() {
                Some(it) => it,
                // スタックも空なら何もしない
                None => return Ok(()),
            };
            // 前のスタックに数値があればそれと同じものをスタックに入れる
            self.stack_vec.stack.push(*item);
        } else if tokens[0].to_lowercase() == "help" {
            // helpコマンド受付
            self.help()?;
            return Ok(());
        }

        // 計算処理へ
        let inner_token = tokens.iter().map(|x| x.to_string()).collect();
        self.calc_inner(inner_token)
    }

    fn calc_inner(&mut self, inner_tokens: Vec<String>) -> Result<()> {
        let mut tokens = inner_tokens;
        // トークンが空になるまで計算処理を行う
        while let Some(in_token) = tokens.pop() {
            let mut token = in_token;
            let mut ctoken: Vec<char> = token.chars().collect();
            // N進法記述処理
            let mut is_nbase = false;
            let ctoken_len = ctoken.len();

            if ctoken_len > 1 {
                let head = ctoken[0];
                let second = ctoken[1];
                let tail = ctoken.last().expect("Could not get chars last");

                if matches!(head, 'B' | 'O' | 'D' | 'H')
                    && (second.is_ascii_digit() || matches!(second, 'a'..='f'))
                {
                    self.basenum_convert(&token)?;

                    is_nbase = true;
                } else if ctoken[ctoken_len - 2] != '/'
                    && matches!(tail, '+' | '-' | '/' | '*' | '^')
                {
                    let enzan = ctoken.pop().unwrap();
                    tokens.push(enzan.to_string());
                    token = ctoken.into_iter().collect();
                }
            }

            if let Ok(x) = &token.parse::<f64>() {
                // 数値に変換できたらスタックへ積む
                let num = NumCast::from(*x).expect("Could not NumCast::from");
                self.stack_vec.push(num);
            } else if !is_nbase {
                self.manege_command(&token, &mut tokens)?;
            }

            // トークンの履歴を取得
            if !token.is_empty()
                && !matches!(
                    token.as_str(),
                    "mem" | "func" | "undo" | "u" | "to" | "def" | "c" | "cls" | "clr"
                )
            {
                self.get_token_history(&token);
            }
        }
        // スタックの長さを限定
        let maximum_stack_length = 50;

        if self.stack_vec.stack.len() > maximum_stack_length {
            let message = format!("Stack length was reached max[{}].", maximum_stack_length);

            self.add_result_message(&message);

            self.stack_vec.stack.remove(0);
        }

        Ok(())
    }
    fn manege_command(&mut self, token: &str, tokens: &mut Vec<String>) -> Result<()> {
        match &token.to_lowercase()[..] {
            "read" => {
                if let Some(path) = tokens.pop() {
                    self.mem_value.clear();
                    self.func_mem.clear();
                    self.stack_vec.clear();
                    self.read_token = readfile(&path)?;
                } else {
                    bail!("too few arguments.");
                }
            }

            "dsfix" => {
                if let Some(point_num) = tokens.pop() {
                    self.display_format = DispFormat::Fix;
                    let pnum = point_num.to_string();
                    self.disp_pointnum = pnum.parse()?;
                } else {
                    bail!("too few arguments.");
                }
            }

            "dseng" => {
                if let Some(point_num) = tokens.pop() {
                    self.display_format = DispFormat::Eng;
                    let pnum = point_num.to_string();
                    self.disp_pointnum = pnum.parse()?;
                } else {
                    bail!("too few arguments.");
                }
            }

            "dsall" => {
                self.display_format = DispFormat::All;
            }

            "sto" | "to" => {
                if let Some(key) = tokens.pop() {
                    if !self.is_ok_mem_string(&key, false) {
                        bail!("This memory key is not available.")
                    }
                    match self.stack_vec.stack.pop() {
                        Some(value) => {
                            match self.mem_value.insert(key.to_string(), value) {
                                // 既にkeyが使われていれば値が返ってくるので上書きされた事を知らせる。
                                Some(item) => self.add_result_message(&format!(
                                    "{}:{:?} was overriden by {:?}.",
                                    key, item, value
                                )),
                                None => self.add_result_message(&format!(
                                    "{:?} is stocked to {}.",
                                    value, key
                                )),
                            }
                        }
                        None => {
                            bail!("invalid Syntax at memory value.");
                        }
                    }
                } else {
                    bail!("invalid Syntax at memory value.");
                }
            }

            "mdel" => match tokens.pop() {
                Some(key) => {
                    if self.mem_value.remove(&key).is_some() {
                        self.add_result_message(&format!("{} is removed", key));
                    } else {
                        bail!("Such name is none in memos")
                    }
                }
                None => {
                    bail!("invalid Syntax at delete memo.");
                }
            },

            "def" | "fn" => match tokens.pop() {
                Some(key) => {
                    if !self.is_ok_mem_string(&key, true) {
                        bail!("This func key is not available.")
                    };

                    // 残りのトークンを反転
                    tokens.reverse();

                    // 文字を連結
                    let value = tokens.join(" ");
                    if value.is_empty() {
                        bail!("invalid Syntax at define function.");
                    };

                    match self.func_mem.insert(key.to_string(), value.to_string()) {
                        Some(item) => {
                            self.remark_message =
                                format!("{}:{:?} was overriden by {:?}.", key, item, value)
                        }
                        None => self.remark_message = format!("{}:{:?} is defined.", key, value),
                    };

                    tokens.clear();
                }

                None => {
                    bail!("invalid Syntax at funcs value.");
                }
            },

            "repeat" | "rp" => match self.stack_vec.pop() {
                Some(n) => {
                    let repnum = n.to_usize().expect("Could not cast to usize");
                    if tokens.is_empty() {
                        bail!("no command for repeat")
                    };

                    let mut reptokens = tokens.to_owned();

                    reptokens.reverse();

                    let reptoken = reptokens.join(" ");

                    for _ in 0..repnum - 1 {
                        self.calc(&reptoken)?
                    }
                }

                None => {
                    bail!("repeat number is none.");
                }
            },

            "roll" => match self.stack_vec.pop() {
                Some(n) => match n.round().to_usize() {
                    Some(n_usize) => {
                        let rollnum = n_usize - 1;
                        let lastnum = self.stack_vec.stack.len() - 1;

                        if rollnum <= lastnum {
                            self.stack_vec.stack.swap(lastnum - rollnum, lastnum);
                        } else {
                            self.stack_vec.stack.push(n);

                            bail!("Invalid syntax at roll command.");
                        }
                    }

                    None => {
                        bail!("roll argument is not integer.");
                    }
                },

                None => {
                    bail!("repeat number is none.");
                }
            },

            "fdel" => match tokens.pop() {
                Some(key) => match self.func_mem.remove(&key) {
                    Some(_) => {
                        self.add_result_message(&format!("func:{} is removed", key));
                    }
                    None => bail!("Such name is none in functions"),
                },

                None => {
                    bail!("invalid Syntax at delete function.");
                }
            },

            _ => {
                if self.mem_value.contains_key(token) {
                    // メモに格納した値があればスタックに収納
                    self.stack_vec.push(self.mem_value[token]);
                } else if self.func_mem.contains_key(token) {
                    // 定義した関数であればtokenを実行
                    let functoken = self.func_mem[token].to_owned();
                    self.calc(&functoken)?;
                } else if self.no_pop_func.contains(&token.to_string()) {
                    // no pop funcのワードに含まれていればその処理へ
                    self.no_pop_functions(&token)?
                } else if self.single_functions.contains(&token.to_string()) {
                    // single pop funcのワードに含まれていればその処理へ
                    self.single_pop_function(&token)?;
                } else if self.stack_vec.stack.len() >= 2 {
                    // その他演算子でスタック要素2個以上あればその処理へ
                    self.two_pop_functions(&token)?;
                } else {
                    bail!("Invalid Syntax");
                }
            }
        };
        Ok(())
    }

    fn basenum_convert(&mut self, ctoken: &str) -> Result<()> {
        if ctoken.len() > 1 {
            let head = &ctoken[0..1];
            let tail = &ctoken[1..];

            let radix = match head {
                "B" => 2,
                "O" => 8,
                "D" => 10,
                "H" => 16,
                _ => bail!("Invalid Syntax at convert Base number"),
            };

            let xnum = <i32 as Num>::from_str_radix(tail, radix)?;
            let num = NumCast::from(xnum).expect("NumCast from ");

            self.stack_vec.push(num);
        }
        Ok(())
    }

    fn no_pop_functions(&mut self, token: &str) -> Result<()> {
        match token {
            "clr" | "cls" | "c" => {
                self.stack_vec.clear();
                self.token_history.clear();
                self.add_result_message("Stacks and history are clear;");
            }
            "mclr" | "mcls" => {
                if is_sure("Will you clear memos?")? {
                    self.mem_value.clear();
                    self.add_result_message("memory values are clear;");
                }
            }
            "fclr" | "fcls" => {
                if is_sure("Will you clear defined functions?")? {
                    self.func_mem.clear();
                    self.add_result_message("functions values are clear;");
                }
            }
            "e" => self.stack_vec.push(FloatConst::E()),
            "pi" => self.stack_vec.push(num::traits::FloatConst::PI()),
            "primes" | "prs" => match &self.stack_vec.stack.last() {
                Some(bnum) => {
                    if let Some(num) = bnum.to_i32() {
                        let mut resultstr = "".to_string();
                        let mut itemnum = num;
                        let divnum = |n: &mut i32, x: i32| -> i32 {
                            let mut counter = 0;
                            while *n % x == 0 {
                                counter += 1;
                                *n /= x;
                            }
                            counter
                        };

                        for i in 2..itemnum + 1 {
                            if i > num.sqrt() {
                                if resultstr.is_empty() {
                                    resultstr += &format!("{}^1 ", num);
                                }
                                break;
                            }
                            match divnum(&mut itemnum, i) {
                                0 => {}
                                x => {
                                    resultstr += &format!("{}^{} ", i, x);
                                }
                            }
                        }

                        self.add_result_message(&resultstr);
                    } else {
                        bail!("last item is not Integer.or abs(num) >= 2 ^ 31")
                    }
                }
                _ => {
                    bail!("stack has no item");
                }
            },
            "sum" => {
                let sumnum = |x: &Vec<T>| -> T { x.iter().fold(num::zero(), |ac, &x| ac + x) };

                let nums: Vec<T> = self
                    .stack_vec
                    .stack
                    .drain(self.last_stack_length..)
                    .collect();

                if nums.is_empty() {
                    let result = sumnum(&self.stack_vec.stack);
                    self.add_result_message(&format!("sum({:?})", &self.stack_vec.stack));
                    self.stack_vec.clear();
                    self.stack_vec.push(result);
                } else {
                    self.stack_vec.push(sumnum(&nums));
                    self.add_result_message(&format!("sum({:?})", &nums));
                }
            }
            "avg" => {
                let length = self.stack_vec.len();

                self.no_pop_functions("sum")?;
                self.stack_vec.push(length);
                self.two_pop_functions("/")?;

                if self.stack_vec.stack.is_empty() {
                    self.add_result_message("Stack is None");
                } else {
                    self.add_result_message(&format!("/ {:?})", &length));
                }
            }
            "rad" => {
                self.angle_mode = AngleMode::Rad;
                self.add_result_message(&format!("anglemode:{:?};", self.angle_mode));
            }
            "deg" => {
                self.angle_mode = AngleMode::Deg;
                self.add_result_message(&format!("anglemode:{:?};", self.angle_mode));
            }
            "swap" | "sw" => {
                let n = self.stack_vec.stack.len();

                if n <= 1 {
                    bail!("stack has not enough length");
                }

                self.stack_vec.stack.swap(n - 2, n - 1);
            }
            "undo" | "u" => {
                self.stack_vec.undo()?;
            }
            "allcls" | "allclr" => self.calc("mclr fclr clr")?,
            "bin" => {
                self.remark_message = "BIN Mode: example B101".to_string();
                self.base_num = BaseNum::Bin;
            }
            "oct" => {
                self.remark_message = "OCT Mode: example O11".to_string();
                self.base_num = BaseNum::Oct;
            }
            "dec" => {
                self.base_num = BaseNum::Dec;
            }
            "hex" => {
                self.remark_message = "HEX Mode: example Hff".to_string();
                self.base_num = BaseNum::Hex;
            }
            _ => bail!("Invalid coding: no argument functions"),
        }
        Ok(())
    }

    fn single_pop_function(&mut self, token: &str) -> Result<()> {
        if let "del" | "dd" = token {
            // 成功していればスタック要素を１つ削除
            match self.stack_vec.pop() {
                Some(dlitem) => {
                    self.add_result_message(&format!("Deleted last item: {:?};", dlitem));
                }
                None => bail!("stack does not have enough length to delete."),
            }
        } else {
            match self.stack_vec.stack.last() {
                Some(n) => {
                    let num = *n;
                    let result = self.single_inner_function(token, num)?;
                    self.add_result_message(&format!("{:?} {}", num, token));
                    self.stack_vec.pop();
                    self.stack_vec.push(result);
                }
                None => {
                    bail!("Syntax Error at function with one argument.");
                }
            }
        };
        Ok(())
    }
    fn single_inner_function(&mut self, token: &str, n: T) -> Result<T> {
        let angle = match self.angle_mode {
            AngleMode::Deg => n.to_radians(),
            AngleMode::Rad => n,
        };
        let conv_angle = |x: T| match self.angle_mode {
            AngleMode::Deg => x.to_degrees(),
            AngleMode::Rad => x,
        };
        let result = match token {
            "sin" => angle.sin(),
            "cos" => angle.cos(),
            "tan" => angle.tan(),
            "asin" => conv_angle(n.asin()),
            "acos" => conv_angle(n.acos()),
            "atan" => conv_angle(n.atan()),
            "sq" => n.mul(n),

            "sqrt" => {
                if n.is_sign_negative() {
                    self.remark_message = "negative sqrt is not supported.".to_string();
                    n
                } else {
                    n.sqrt()
                }
            }

            "todeg" => n.to_degrees(),
            "torad" => n.to_radians(),
            "log" => n.log10(),
            "abs" => n.abs(),
            "round" => n.round(),
            "1/x" => n.recip(),
            "-x" => n.neg(),

            "frac" | "!" => NumCast::from(frac(n)?).expect("Error at NumCast::from()"),
            _ => {
                bail!("Invaid coding in functions: function with one argument.")
            }
        };
        Ok(result)
    }

    fn two_pop_functions(&mut self, token: &str) -> Result<()> {
        let lastnum = self.stack_vec.stack.len();
        let a: T = self.stack_vec.stack[lastnum - 1];
        let b: T = self.stack_vec.stack[lastnum - 2];
        let result = self.two_pop_inner_function(token, a, b)?;
        self.add_result_message(&format!("{:?} {:?} {}", b, a, token));
        // 成功していればスタック要素を２つ削除
        (0..2).for_each(|_| {
            self.stack_vec.stack.pop();
        });

        self.stack_vec.push(result);
        Ok(())
    }

    fn two_pop_inner_function(&mut self, token: &str, a: T, b: T) -> Result<T> {
        let cast_err_msg = "Could not cast number at two pop function.";
        let result = match token {
            "+" => b.add(a),
            "-" => b.sub(a),
            "*" => b.mul(a),
            "/" | "div" => b.div(a),
            "^" | "pow" => b.powf(a),
            "logx" => b.log(a),
            "mod" | "%" => b.rem(a),
            "max" => b.max(a),
            "min" => b.min(a),
            "nrt" => {
                if b.is_sign_negative() {
                    self.remark_message = "negative nst root is not supported.".to_string();
                    b
                } else {
                    b.powf(a.recip())
                }
            }
            "//" => {
                if a.is_zero() {
                    bail!("error. divied by 0.");
                }
                FromPrimitive::from_i64(
                    b.to_i64()
                        .expect(cast_err_msg)
                        .div_euclid(a.to_i64().expect(cast_err_msg)),
                )
                .unwrap()
            }
            "and" => {
                judinteger(a)?;
                judinteger(b)?;

                NumCast::from(b.to_i32().expect(cast_err_msg) & a.to_i32().expect(cast_err_msg))
                    .expect("Error at NumCast::from")
            }
            "or" => {
                judinteger(a)?;
                judinteger(b)?;

                NumCast::from(b.to_i32().expect(cast_err_msg) | a.to_i32().expect(cast_err_msg))
                    .expect("Error at NumCast::from")
            }

            "xor" => {
                judinteger(a)?;
                judinteger(b)?;

                NumCast::from(b.to_i32().expect(cast_err_msg) ^ a.to_i32().expect(cast_err_msg))
                    .expect("Error at NumCast::from")
            }
            "<<" => {
                judinteger(a)?;
                judinteger(b)?;

                if let Some(result) =
                    NumCast::from(b.to_i32().expect("overflow") << a.to_i32().expect("overflow"))
                {
                    result
                } else {
                    bail!("overflow at shift calculation.")
                }
            }
            ">>" => {
                judinteger(a)?;
                judinteger(b)?;

                match NumCast::from(b.to_i32().expect("overflow") >> a.to_i32().expect("overflow"))
                {
                    Some(result) => result,
                    None => bail!("overflow at shift calculation."),
                }
            }
            "npr" => {
                // nPr = (n!) / (n-r)!
                if a.to_i32() > Some(30000) || b.to_i32() > Some(30000) {
                    bail!("num is too big to calc nPr")
                };
                judinteger(a)?;
                judinteger(b)?;
                if a > b {
                    Zero::zero()
                } else {
                    NumCast::from(frac(b)? / frac(b - a)?).expect("Error at NumCast::from")
                }
            }

            "ncr" => {
                // nCr = n! / (r!(n-r)!)
                if a.to_i32() > Some(30000) || b.to_i32() > Some(30000) {
                    bail!("num is too big to calc nCr")
                };
                judinteger(a)?;
                judinteger(b)?;
                if a > b {
                    return Ok(Zero::zero());
                }

                NumCast::from(frac(b)? / (frac(a)? * frac(b - a)?)).expect("Error at NumCast::from")
            }

            "lcm" => {
                judinteger(a)?;
                judinteger(b)?;
                let a_i32 = a.to_i32().expect(cast_err_msg);
                let b_i32 = b.to_i32().expect(cast_err_msg);
                let mut i: i32 = One::one();
                let result: T;
                loop {
                    let mulnum = a_i32.max(b_i32) * i;
                    let divnum = a_i32.min(b_i32);
                    if mulnum % divnum == 0 {
                        self.add_result_message(&format!("lcm ({:?},{:?})", a, b));
                        result = NumCast::from(mulnum).expect("Error at NumCast::from");
                        break;
                    } else {
                        i += 1;
                    }
                }
                result
            }
            "gcm" => {
                judinteger(a)?;
                judinteger(b)?;
                let a_i32 = a.to_i32().expect(cast_err_msg);
                let b_i32 = b.to_i32().expect(cast_err_msg);
                let snum = a_i32.min(b_i32);
                let mnum = a_i32.max(b_i32);
                let mut result = One::one();

                for i in 1..snum {
                    if snum % i == 0 {
                        let ssnum = snum / i;
                        if mnum % ssnum == 0 {
                            result = NumCast::from(ssnum).expect("Error at NumCast::from");
                            break;
                        }
                    }
                }
                self.add_result_message(&format!("gcm ({:?},{:?})", a, b));
                result
            }
            _ => {
                bail!("Invalid syntax: too few arguments.");
            }
        };
        Ok(result)
    }

    fn help(&self) -> Result<()> {
        let mut help_buff = BufWriter::new(stdout());
        writeln!(help_buff, "functions:")?;

        let mut writer = |vc: Vec<String>| {
            let rtnum = 8;
            let mut i = 0;
            vc.iter().for_each(|val| {
                if i % rtnum == 0 && i != 0 {
                    writeln!(help_buff).expect("Cannot writeln!");
                }
                write!(help_buff, "{}\t", val).expect("Cannot write!");
                i += 1;
            })
        };

        let mut helper_func = [
            self.another_func.to_owned(),
            self.single_functions.to_owned(),
            self.no_pop_func.to_owned(),
        ]
        .concat();

        helper_func.sort();
        writer(helper_func);

        writeln!(
            help_buff,
            "\n\n'to' or 'sto' <space> 'string': memo to string."
        )?;

        writeln!(
            help_buff,
            "'def' or 'fn' <space> name <space> token: function entry."
        )?;

        writeln!(
            help_buff,
            "\npress q to exit. press help to see functions name.\n"
        )?;

        writeln!(help_buff, "exmple of [bin,oct,hex] -> [B101 O51 Hff]\n")?;

        writeln!(help_buff, "read [filename]: read textfile")?;
        help_buff.flush()?;

        let mut rl = DefaultEditor::new()?;
        let _readline = rl.readline("Press enter key.")?;

        Ok(())
    }

    fn get_token_history(&mut self, token: &str) {
        // トークンの履歴を追加
        let mut item = vec![token.to_string()];
        item.append(&mut self.token_history);
        item.truncate(15);
        self.token_history = item;
    }
}

fn is_sure(message: &str) -> Result<bool> {
    let mut rl = DefaultEditor::new()?;
    let mut answer: Option<bool> = None;
    while answer.is_none() {
        let msg = format!("{}:[yes/no]>>", message);
        let readline = rl.readline(&msg).expect("Error at readline()");
        answer = match &readline.to_lowercase()[..] {
            "yes" | "y" => Some(true),
            "no" | "n" => Some(false),
            _ => None,
        };
    }
    Ok(answer.unwrap())
}

fn judinteger<T: Num + NumCast>(x: T) -> Result<()> {
    if let Some(num) = x.to_isize() {
        if x != NumCast::from(num).expect("Error at NumCast::from() in judinteger") {
            bail!("item is not integer")
        }
    } else {
        bail!("item can not cast to isize")
    }
    Ok(())
}

fn frac<T: Num + NumCast + Copy>(x: T) -> Result<BigInt> {
    judinteger(x)?;
    if x.to_i32() > Some(30000) {
        bail!("num is too big to calc Factorial")
    }
    let mut sn: BigInt = One::one();
    let xnum = x.to_usize().expect("Error at to_usize() in frac");

    for i in 1..=xnum {
        sn *= i;
    }
    Ok(sn)
}

fn str_to_vec(st: &str) -> Vec<String> {
    // 文字リテラルからVecへ変換
    let result = st.split_whitespace().map(|x| x.to_owned()).collect();
    result
}

fn convnum<T: Num + Clone>(app: &RpnCalculator<T>, x: &f64) -> String {
    let number_i32 = *x as i32;
    let number_f64 = *x;
    match app.base_num {
        BaseNum::Dec => {
            let dspoint = &app.disp_pointnum;
            let dsnum = number_f64;
            match app.display_format {
                DispFormat::Fix => format!("{:.*} ", dspoint, dsnum),
                DispFormat::Eng => format!("{:.*E} ", dspoint, dsnum),
                DispFormat::All => format!("{} ", dsnum),
            }
        }
        BaseNum::Bin => format!("B:{:b}  := {} ", number_i32, number_i32),
        BaseNum::Oct => format!("O:{:o}  := {} ", number_i32, number_i32),
        BaseNum::Hex => format!("H:{:x}  := {} ", number_i32, number_i32),
    }
}

#[cfg(test)]
#[test]
fn test_calc() {
    // テストで使用する関数内関数
    let testcalc = |token: &str, ans: f64| -> bool {
        // token:計算トークン　ans:その正解の値
        // tokenに対して正解の値になっているかのテスト
        let mut app: RpnCalculator<f64> = RpnCalculator::new();
        app.angle_mode = AngleMode::Rad;
        app.calc(token).ok();

        // テスト内で使用するトークンを処理するクロージャー
        let result = app
            .stack_vec
            .stack
            .last()
            .expect("Could not get last element.");

        result == &ans
    };

    // 以下テスト処理
    // 角度はradモードにしている
    assert!(testcalc("2 3 +", 5.0));
    assert!(testcalc("2 3 -", -1.0));
    assert!((testcalc("2 3 *", 6.0)));
    assert!((testcalc("3 2 /", 1.5)));
    assert!((testcalc("3 2 ^", 9.0)));
    assert!((testcalc("9 sqrt", 3.0)));
    assert!((testcalc("8 3 nrt", 2.0)));
    assert!(testcalc("2 3 8 3 + - +", -6.0));
    assert!(testcalc("60 torad cos 120 * round", 60.0));
    assert!(testcalc("pi 2 / sin round", 1.0));
    assert!(testcalc("pi cos round", -1.0));
    assert!(testcalc("pi 4 / tan round", 1.0));
    assert!(testcalc("1 asin todeg", 90.0));
    assert!(testcalc("-1 acos todeg", 180.0));
    assert!(testcalc("1 atan todeg", 45.0));
    assert!(testcalc("-5 abs", 5.0));
    assert!(testcalc("2 1/x", 0.5));
    assert!(testcalc("5 3 mod", 2.0));
    assert!(testcalc("5 2 //", 2.0));
    assert!(testcalc("9 2 pow", 81.0));
    assert!(testcalc("2 sq 9 sqrt +", 7.0));
    assert!(testcalc("9 2 ^", 81.0));
    assert!(testcalc("100 log", 2.0));
    assert!(testcalc("8 2 logx", 3.0));
    assert!(testcalc("3 9 sw -", 6.0));
    assert!(testcalc("3 4 5 sum", 12.0));
    assert!(testcalc("4 2 8 6 avg", 5.0));
    assert!(testcalc("218 sto x x 2 / ", 109.0));
    assert!(testcalc("3 1 2 swap", 1.0));
    assert!(testcalc("1 2 3 del", 2.0));
    assert!(testcalc("e", num::traits::FloatConst::E()));
    assert!(testcalc("pi", num::traits::FloatConst::PI()));
    assert!(testcalc("1 2 3 4 5 cls 9 sum", 9.0));
    assert!(testcalc("2 pi * 50 * 100 + 60 torad cos * round", 207.0));
    assert!(testcalc("1 5 rp 2 +", 11.0));
    assert!(testcalc("1 2 3 + +", 6.0));
    assert!(testcalc("6 -x", -6.0));
    assert!(testcalc("5 1/x", 0.2));
    assert!(testcalc("B1111", 15.0));
    assert!(testcalc("B0011", 3.0));
    assert!(testcalc("bin B101 B1110 and", 4.0));
    assert!(testcalc("bin B1010 B0101 or", 15.0));
    assert!(testcalc("bin B0101 B1111 xor", 10.0));
    assert!(testcalc("bin B0101 2 << ", 20.0));
    assert!(testcalc("bin B010100 2 >> ", 5.0));
    assert!(testcalc("O1111", 585.0));
    assert!(testcalc("Ha", 10.0));
    assert!(testcalc("H249", 585.0));
    assert!(testcalc("1 2 max", 2.0));
    assert!(testcalc("1 2 min", 1.0));
    assert!(testcalc("e sq e logx", 2.0));
    assert!(testcalc("50 3 ncr", 19600.0));
    assert!(testcalc("50 3 npr", 117600.0));
    assert!(testcalc("5 !", 120.0));
    assert!(testcalc("5 3-", 2.0));
    assert!(testcalc("2 3^", 8.0));
    assert!(testcalc("2 3^ 8+", 16.0));
    assert!(testcalc("6 2/", 3.0));
}
