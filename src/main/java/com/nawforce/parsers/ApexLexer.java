// Generated from /Users/kevin/Projects/ApexLink/src/main/antlr4/com/nawforce/parsers/ApexLexer.g4 by ANTLR 4.7.2
package com.nawforce.parsers;

import java.util.*; 

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ApexLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ABSTRACT=1, BOOLEAN=2, BREAK=3, BYTE=4, CATCH=5, CHAR=6, CLASS=7, CONST=8, 
		CONTINUE=9, DEFAULT=10, DO=11, DOUBLE=12, ELSE=13, ENUM=14, EXTENDS=15, 
		FINAL=16, FINALLY=17, FLOAT=18, FOR=19, IF=20, GOTO=21, IMPLEMENTS=22, 
		INSTANCEOF=23, INTERFACE=24, LONG=25, NATIVE=26, NEW=27, PACKAGE=28, PRIVATE=29, 
		PROTECTED=30, PUBLIC=31, RETURN=32, SHORT=33, STATIC=34, VIRTUAL=35, SUPER=36, 
		SYNCHRONIZED=37, THIS=38, THROW=39, TRANSIENT=40, TRY=41, VOID=42, VOLATILE=43, 
		WHILE=44, GLOBAL=45, WEBSERVICE=46, SELECT=47, INSERT=48, UPSERT=49, UPDATE=50, 
		DELETE=51, UNDELETE=52, MERGE=53, TESTMETHOD=54, OVERRIDE=55, GET=56, 
		SET=57, BLOB=58, DATE=59, DATETIME=60, DECIMAL=61, ID=62, INTEGER=63, 
		OBJECT=64, STRING=65, TIME=66, RUNAS=67, WITH=68, WITHOUT=69, SHARING=70, 
		WITHSHARING=71, WITHOUTSHARING=72, IntegerLiteral=73, NumberLiteral=74, 
		BooleanLiteral=75, StringLiteral=76, NullLiteral=77, LPAREN=78, RPAREN=79, 
		LBRACE=80, RBRACE=81, LBRACK=82, RBRACK=83, SEMI=84, COMMA=85, DOT=86, 
		ASSIGN=87, LE=88, GE=89, GT=90, LT=91, BANG=92, TILDE=93, QUESTION=94, 
		COLON=95, EQUAL=96, TRIPLEEQUAL=97, NOTEQUAL=98, LESSANDGREATER=99, TRIPLENOTEQUAL=100, 
		AND=101, OR=102, INC=103, DEC=104, ADD=105, SUB=106, MUL=107, DIV=108, 
		BITAND=109, BITOR=110, CARET=111, MOD=112, MAP=113, ADD_ASSIGN=114, SUB_ASSIGN=115, 
		MUL_ASSIGN=116, DIV_ASSIGN=117, AND_ASSIGN=118, OR_ASSIGN=119, XOR_ASSIGN=120, 
		MOD_ASSIGN=121, LSHIFT_ASSIGN=122, RSHIFT_ASSIGN=123, URSHIFT_ASSIGN=124, 
		Identifier=125, AT=126, ELLIPSIS=127, WS=128, BANG_STATEMENT=129, DOC_COMMENT=130, 
		COMMENT=131, LINE_COMMENT=132;
	public static final int
		WHITESPACE_CHANNEL=2, COMMENT_CHANNEL=3;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN", "WHITESPACE_CHANNEL", "COMMENT_CHANNEL"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"ABSTRACT", "BOOLEAN", "BREAK", "BYTE", "CATCH", "CHAR", "CLASS", "CONST", 
			"CONTINUE", "DEFAULT", "DO", "DOUBLE", "ELSE", "ENUM", "EXTENDS", "FINAL", 
			"FINALLY", "FLOAT", "FOR", "IF", "GOTO", "IMPLEMENTS", "INSTANCEOF", 
			"INTERFACE", "LONG", "NATIVE", "NEW", "PACKAGE", "PRIVATE", "PROTECTED", 
			"PUBLIC", "RETURN", "SHORT", "STATIC", "VIRTUAL", "SUPER", "SYNCHRONIZED", 
			"THIS", "THROW", "TRANSIENT", "TRY", "VOID", "VOLATILE", "WHILE", "GLOBAL", 
			"WEBSERVICE", "SELECT", "INSERT", "UPSERT", "UPDATE", "DELETE", "UNDELETE", 
			"MERGE", "TESTMETHOD", "OVERRIDE", "GET", "SET", "BLOB", "DATE", "DATETIME", 
			"DECIMAL", "ID", "INTEGER", "OBJECT", "STRING", "TIME", "RUNAS", "WITH", 
			"WITHOUT", "SHARING", "WITHSHARING", "WITHOUTSHARING", "IntegerLiteral", 
			"NumberLiteral", "Digit", "BooleanLiteral", "StringLiteral", "StringCharacters", 
			"StringCharacter", "EscapeSequence", "NullLiteral", "LPAREN", "RPAREN", 
			"LBRACE", "RBRACE", "LBRACK", "RBRACK", "SEMI", "COMMA", "DOT", "ASSIGN", 
			"LE", "GE", "GT", "LT", "BANG", "TILDE", "QUESTION", "COLON", "EQUAL", 
			"TRIPLEEQUAL", "NOTEQUAL", "LESSANDGREATER", "TRIPLENOTEQUAL", "AND", 
			"OR", "INC", "DEC", "ADD", "SUB", "MUL", "DIV", "BITAND", "BITOR", "CARET", 
			"MOD", "MAP", "ADD_ASSIGN", "SUB_ASSIGN", "MUL_ASSIGN", "DIV_ASSIGN", 
			"AND_ASSIGN", "OR_ASSIGN", "XOR_ASSIGN", "MOD_ASSIGN", "LSHIFT_ASSIGN", 
			"RSHIFT_ASSIGN", "URSHIFT_ASSIGN", "Identifier", "JavaLetter", "JavaLetterOrDigit", 
			"AT", "ELLIPSIS", "WS", "BANG_STATEMENT", "DOC_COMMENT", "COMMENT", "LINE_COMMENT"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'abstract'", "'boolean'", "'break'", "'byte'", "'catch'", "'char'", 
			"'class'", "'const'", "'continue'", "'default'", "'do'", "'double'", 
			"'else'", "'enum'", "'extends'", "'final'", "'finally'", "'float'", "'for'", 
			"'if'", "'goto'", "'implements'", "'instanceof'", "'interface'", "'long'", 
			"'native'", "'new'", "'package'", "'private'", "'protected'", "'public'", 
			"'return'", "'short'", "'static'", "'virtual'", "'super'", "'synchronized'", 
			"'this'", "'throw'", "'transient'", "'try'", "'void'", "'volatile'", 
			"'while'", "'global'", "'webservice'", "'select'", "'insert'", "'upsert'", 
			"'update'", "'delete'", "'undelete'", "'merge'", "'testmethod'", "'override'", 
			"'get'", "'set'", "'blob'", "'date'", "'datetime'", "'decimal'", "'id'", 
			"'integer'", "'object'", "'string'", "'time'", "'system.runas'", "'with'", 
			"'without'", "'sharing'", "'withsharing'", "'withoutsharing'", null, 
			null, null, null, "'null'", "'('", "')'", "'{'", "'}'", "'['", "']'", 
			"';'", "','", "'.'", "'='", "'<='", "'>='", "'>'", "'<'", "'!'", "'~'", 
			"'?'", "':'", "'=='", "'==='", "'!='", "'<>'", "'!=='", "'&&'", "'||'", 
			"'++'", "'--'", "'+'", "'-'", "'*'", "'/'", "'&'", "'|'", "'^'", "'%'", 
			"'=>'", "'+='", "'-='", "'*='", "'/='", "'&='", "'|='", "'^='", "'%='", 
			"'<<='", "'>>='", "'>>>='", null, "'@'", "'...'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ABSTRACT", "BOOLEAN", "BREAK", "BYTE", "CATCH", "CHAR", "CLASS", 
			"CONST", "CONTINUE", "DEFAULT", "DO", "DOUBLE", "ELSE", "ENUM", "EXTENDS", 
			"FINAL", "FINALLY", "FLOAT", "FOR", "IF", "GOTO", "IMPLEMENTS", "INSTANCEOF", 
			"INTERFACE", "LONG", "NATIVE", "NEW", "PACKAGE", "PRIVATE", "PROTECTED", 
			"PUBLIC", "RETURN", "SHORT", "STATIC", "VIRTUAL", "SUPER", "SYNCHRONIZED", 
			"THIS", "THROW", "TRANSIENT", "TRY", "VOID", "VOLATILE", "WHILE", "GLOBAL", 
			"WEBSERVICE", "SELECT", "INSERT", "UPSERT", "UPDATE", "DELETE", "UNDELETE", 
			"MERGE", "TESTMETHOD", "OVERRIDE", "GET", "SET", "BLOB", "DATE", "DATETIME", 
			"DECIMAL", "ID", "INTEGER", "OBJECT", "STRING", "TIME", "RUNAS", "WITH", 
			"WITHOUT", "SHARING", "WITHSHARING", "WITHOUTSHARING", "IntegerLiteral", 
			"NumberLiteral", "BooleanLiteral", "StringLiteral", "NullLiteral", "LPAREN", 
			"RPAREN", "LBRACE", "RBRACE", "LBRACK", "RBRACK", "SEMI", "COMMA", "DOT", 
			"ASSIGN", "LE", "GE", "GT", "LT", "BANG", "TILDE", "QUESTION", "COLON", 
			"EQUAL", "TRIPLEEQUAL", "NOTEQUAL", "LESSANDGREATER", "TRIPLENOTEQUAL", 
			"AND", "OR", "INC", "DEC", "ADD", "SUB", "MUL", "DIV", "BITAND", "BITOR", 
			"CARET", "MOD", "MAP", "ADD_ASSIGN", "SUB_ASSIGN", "MUL_ASSIGN", "DIV_ASSIGN", 
			"AND_ASSIGN", "OR_ASSIGN", "XOR_ASSIGN", "MOD_ASSIGN", "LSHIFT_ASSIGN", 
			"RSHIFT_ASSIGN", "URSHIFT_ASSIGN", "Identifier", "AT", "ELLIPSIS", "WS", 
			"BANG_STATEMENT", "DOC_COMMENT", "COMMENT", "LINE_COMMENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public ApexLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ApexLexer.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 129:
			return JavaLetter_sempred((RuleContext)_localctx, predIndex);
		case 130:
			return JavaLetterOrDigit_sempred((RuleContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean JavaLetter_sempred(RuleContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return Character.isJavaIdentifierStart(_input.LA(-1));
		case 1:
			return Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)));
		}
		return true;
	}
	private boolean JavaLetterOrDigit_sempred(RuleContext _localctx, int predIndex) {
		switch (predIndex) {
		case 2:
			return Character.isJavaIdentifierPart(_input.LA(-1));
		case 3:
			return Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)));
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\u0086\u042b\b\1\4"+
		"\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n"+
		"\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t"+
		"+\4,\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64"+
		"\t\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t"+
		"=\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4"+
		"I\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\t"+
		"T\4U\tU\4V\tV\4W\tW\4X\tX\4Y\tY\4Z\tZ\4[\t[\4\\\t\\\4]\t]\4^\t^\4_\t_"+
		"\4`\t`\4a\ta\4b\tb\4c\tc\4d\td\4e\te\4f\tf\4g\tg\4h\th\4i\ti\4j\tj\4k"+
		"\tk\4l\tl\4m\tm\4n\tn\4o\to\4p\tp\4q\tq\4r\tr\4s\ts\4t\tt\4u\tu\4v\tv"+
		"\4w\tw\4x\tx\4y\ty\4z\tz\4{\t{\4|\t|\4}\t}\4~\t~\4\177\t\177\4\u0080\t"+
		"\u0080\4\u0081\t\u0081\4\u0082\t\u0082\4\u0083\t\u0083\4\u0084\t\u0084"+
		"\4\u0085\t\u0085\4\u0086\t\u0086\4\u0087\t\u0087\4\u0088\t\u0088\4\u0089"+
		"\t\u0089\4\u008a\t\u008a\4\u008b\t\u008b\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3"+
		"\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5"+
		"\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3"+
		"\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n"+
		"\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3"+
		"\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\3\21\3\21\3\21\3\21\3\21\3\21\3\22\3\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23\3\24\3\24"+
		"\3\24\3\24\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\3\27\3\27\3\27\3\27"+
		"\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30\3\30\3\30"+
		"\3\30\3\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\32\3\32\3\32\3\32\3\32\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\34\3\34"+
		"\3\34\3\34\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\36\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37"+
		"\3 \3 \3 \3 \3 \3 \3 \3!\3!\3!\3!\3!\3!\3!\3\"\3\"\3\"\3\"\3\"\3\"\3#"+
		"\3#\3#\3#\3#\3#\3#\3$\3$\3$\3$\3$\3$\3$\3$\3%\3%\3%\3%\3%\3%\3&\3&\3&"+
		"\3&\3&\3&\3&\3&\3&\3&\3&\3&\3&\3\'\3\'\3\'\3\'\3\'\3(\3(\3(\3(\3(\3(\3"+
		")\3)\3)\3)\3)\3)\3)\3)\3)\3)\3*\3*\3*\3*\3+\3+\3+\3+\3+\3,\3,\3,\3,\3"+
		",\3,\3,\3,\3,\3-\3-\3-\3-\3-\3-\3.\3.\3.\3.\3.\3.\3.\3/\3/\3/\3/\3/\3"+
		"/\3/\3/\3/\3/\3/\3\60\3\60\3\60\3\60\3\60\3\60\3\60\3\61\3\61\3\61\3\61"+
		"\3\61\3\61\3\61\3\62\3\62\3\62\3\62\3\62\3\62\3\62\3\63\3\63\3\63\3\63"+
		"\3\63\3\63\3\63\3\64\3\64\3\64\3\64\3\64\3\64\3\64\3\65\3\65\3\65\3\65"+
		"\3\65\3\65\3\65\3\65\3\65\3\66\3\66\3\66\3\66\3\66\3\66\3\67\3\67\3\67"+
		"\3\67\3\67\3\67\3\67\3\67\3\67\3\67\3\67\38\38\38\38\38\38\38\38\38\3"+
		"9\39\39\39\3:\3:\3:\3:\3;\3;\3;\3;\3;\3<\3<\3<\3<\3<\3=\3=\3=\3=\3=\3"+
		"=\3=\3=\3=\3>\3>\3>\3>\3>\3>\3>\3>\3?\3?\3?\3@\3@\3@\3@\3@\3@\3@\3@\3"+
		"A\3A\3A\3A\3A\3A\3A\3B\3B\3B\3B\3B\3B\3B\3C\3C\3C\3C\3C\3D\3D\3D\3D\3"+
		"D\3D\3D\3D\3D\3D\3D\3D\3D\3E\3E\3E\3E\3E\3F\3F\3F\3F\3F\3F\3F\3F\3G\3"+
		"G\3G\3G\3G\3G\3G\3G\3H\3H\3H\3H\3H\3H\3H\3H\3H\3H\3H\3H\3I\3I\3I\3I\3"+
		"I\3I\3I\3I\3I\3I\3I\3I\3I\3I\3I\3J\3J\7J\u031d\nJ\fJ\16J\u0320\13J\3J"+
		"\5J\u0323\nJ\3K\7K\u0326\nK\fK\16K\u0329\13K\3K\3K\3K\7K\u032e\nK\fK\16"+
		"K\u0331\13K\3K\5K\u0334\nK\3L\3L\3M\3M\3M\3M\3M\3M\3M\3M\3M\5M\u0341\n"+
		"M\3N\3N\5N\u0345\nN\3N\3N\3O\6O\u034a\nO\rO\16O\u034b\3P\3P\5P\u0350\n"+
		"P\3Q\3Q\3Q\3R\3R\3R\3R\3R\3S\3S\3T\3T\3U\3U\3V\3V\3W\3W\3X\3X\3Y\3Y\3"+
		"Z\3Z\3[\3[\3\\\3\\\3]\3]\3]\3^\3^\3^\3_\3_\3`\3`\3a\3a\3b\3b\3c\3c\3d"+
		"\3d\3e\3e\3e\3f\3f\3f\3f\3g\3g\3g\3h\3h\3h\3i\3i\3i\3i\3j\3j\3j\3k\3k"+
		"\3k\3l\3l\3l\3m\3m\3m\3n\3n\3o\3o\3p\3p\3q\3q\3r\3r\3s\3s\3t\3t\3u\3u"+
		"\3v\3v\3v\3w\3w\3w\3x\3x\3x\3y\3y\3y\3z\3z\3z\3{\3{\3{\3|\3|\3|\3}\3}"+
		"\3}\3~\3~\3~\3\177\3\177\3\177\3\177\3\u0080\3\u0080\3\u0080\3\u0080\3"+
		"\u0081\3\u0081\3\u0081\3\u0081\3\u0081\3\u0082\3\u0082\7\u0082\u03d7\n"+
		"\u0082\f\u0082\16\u0082\u03da\13\u0082\3\u0083\3\u0083\3\u0083\3\u0083"+
		"\3\u0083\3\u0083\5\u0083\u03e2\n\u0083\3\u0084\3\u0084\3\u0084\3\u0084"+
		"\3\u0084\3\u0084\5\u0084\u03ea\n\u0084\3\u0085\3\u0085\3\u0086\3\u0086"+
		"\3\u0086\3\u0086\3\u0087\6\u0087\u03f3\n\u0087\r\u0087\16\u0087\u03f4"+
		"\3\u0087\3\u0087\3\u0088\3\u0088\3\u0088\3\u0088\3\u0088\7\u0088\u03fe"+
		"\n\u0088\f\u0088\16\u0088\u0401\13\u0088\3\u0089\3\u0089\3\u0089\3\u0089"+
		"\3\u0089\3\u0089\7\u0089\u0409\n\u0089\f\u0089\16\u0089\u040c\13\u0089"+
		"\3\u0089\3\u0089\3\u0089\3\u0089\3\u0089\3\u008a\3\u008a\3\u008a\3\u008a"+
		"\7\u008a\u0417\n\u008a\f\u008a\16\u008a\u041a\13\u008a\3\u008a\3\u008a"+
		"\3\u008a\3\u008a\3\u008a\3\u008b\3\u008b\3\u008b\3\u008b\7\u008b\u0425"+
		"\n\u008b\f\u008b\16\u008b\u0428\13\u008b\3\u008b\3\u008b\4\u040a\u0418"+
		"\2\u008c\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17"+
		"\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\67\35"+
		"9\36;\37= ?!A\"C#E$G%I&K\'M(O)Q*S+U,W-Y.[/]\60_\61a\62c\63e\64g\65i\66"+
		"k\67m8o9q:s;u<w=y>{?}@\177A\u0081B\u0083C\u0085D\u0087E\u0089F\u008bG"+
		"\u008dH\u008fI\u0091J\u0093K\u0095L\u0097\2\u0099M\u009bN\u009d\2\u009f"+
		"\2\u00a1\2\u00a3O\u00a5P\u00a7Q\u00a9R\u00abS\u00adT\u00afU\u00b1V\u00b3"+
		"W\u00b5X\u00b7Y\u00b9Z\u00bb[\u00bd\\\u00bf]\u00c1^\u00c3_\u00c5`\u00c7"+
		"a\u00c9b\u00cbc\u00cdd\u00cfe\u00d1f\u00d3g\u00d5h\u00d7i\u00d9j\u00db"+
		"k\u00ddl\u00dfm\u00e1n\u00e3o\u00e5p\u00e7q\u00e9r\u00ebs\u00edt\u00ef"+
		"u\u00f1v\u00f3w\u00f5x\u00f7y\u00f9z\u00fb{\u00fd|\u00ff}\u0101~\u0103"+
		"\177\u0105\2\u0107\2\u0109\u0080\u010b\u0081\u010d\u0082\u010f\u0083\u0111"+
		"\u0084\u0113\u0085\u0115\u0086\3\2\16\4\2NNnn\4\2FFff\3\2\62;\4\2))^^"+
		"\n\2$$))^^ddhhppttvv\6\2&&C\\aac|\4\2\2\u0101\ud802\udc01\3\2\ud802\udc01"+
		"\3\2\udc02\ue001\7\2&&\62;C\\aac|\5\2\13\f\16\17\"\"\4\2\f\f\17\17\2\u0437"+
		"\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2"+
		"\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2"+
		"\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2"+
		"\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2"+
		"\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3"+
		"\2\2\2\2=\3\2\2\2\2?\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2"+
		"\2\2I\3\2\2\2\2K\3\2\2\2\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2"+
		"U\3\2\2\2\2W\3\2\2\2\2Y\3\2\2\2\2[\3\2\2\2\2]\3\2\2\2\2_\3\2\2\2\2a\3"+
		"\2\2\2\2c\3\2\2\2\2e\3\2\2\2\2g\3\2\2\2\2i\3\2\2\2\2k\3\2\2\2\2m\3\2\2"+
		"\2\2o\3\2\2\2\2q\3\2\2\2\2s\3\2\2\2\2u\3\2\2\2\2w\3\2\2\2\2y\3\2\2\2\2"+
		"{\3\2\2\2\2}\3\2\2\2\2\177\3\2\2\2\2\u0081\3\2\2\2\2\u0083\3\2\2\2\2\u0085"+
		"\3\2\2\2\2\u0087\3\2\2\2\2\u0089\3\2\2\2\2\u008b\3\2\2\2\2\u008d\3\2\2"+
		"\2\2\u008f\3\2\2\2\2\u0091\3\2\2\2\2\u0093\3\2\2\2\2\u0095\3\2\2\2\2\u0099"+
		"\3\2\2\2\2\u009b\3\2\2\2\2\u00a3\3\2\2\2\2\u00a5\3\2\2\2\2\u00a7\3\2\2"+
		"\2\2\u00a9\3\2\2\2\2\u00ab\3\2\2\2\2\u00ad\3\2\2\2\2\u00af\3\2\2\2\2\u00b1"+
		"\3\2\2\2\2\u00b3\3\2\2\2\2\u00b5\3\2\2\2\2\u00b7\3\2\2\2\2\u00b9\3\2\2"+
		"\2\2\u00bb\3\2\2\2\2\u00bd\3\2\2\2\2\u00bf\3\2\2\2\2\u00c1\3\2\2\2\2\u00c3"+
		"\3\2\2\2\2\u00c5\3\2\2\2\2\u00c7\3\2\2\2\2\u00c9\3\2\2\2\2\u00cb\3\2\2"+
		"\2\2\u00cd\3\2\2\2\2\u00cf\3\2\2\2\2\u00d1\3\2\2\2\2\u00d3\3\2\2\2\2\u00d5"+
		"\3\2\2\2\2\u00d7\3\2\2\2\2\u00d9\3\2\2\2\2\u00db\3\2\2\2\2\u00dd\3\2\2"+
		"\2\2\u00df\3\2\2\2\2\u00e1\3\2\2\2\2\u00e3\3\2\2\2\2\u00e5\3\2\2\2\2\u00e7"+
		"\3\2\2\2\2\u00e9\3\2\2\2\2\u00eb\3\2\2\2\2\u00ed\3\2\2\2\2\u00ef\3\2\2"+
		"\2\2\u00f1\3\2\2\2\2\u00f3\3\2\2\2\2\u00f5\3\2\2\2\2\u00f7\3\2\2\2\2\u00f9"+
		"\3\2\2\2\2\u00fb\3\2\2\2\2\u00fd\3\2\2\2\2\u00ff\3\2\2\2\2\u0101\3\2\2"+
		"\2\2\u0103\3\2\2\2\2\u0109\3\2\2\2\2\u010b\3\2\2\2\2\u010d\3\2\2\2\2\u010f"+
		"\3\2\2\2\2\u0111\3\2\2\2\2\u0113\3\2\2\2\2\u0115\3\2\2\2\3\u0117\3\2\2"+
		"\2\5\u0120\3\2\2\2\7\u0128\3\2\2\2\t\u012e\3\2\2\2\13\u0133\3\2\2\2\r"+
		"\u0139\3\2\2\2\17\u013e\3\2\2\2\21\u0144\3\2\2\2\23\u014a\3\2\2\2\25\u0153"+
		"\3\2\2\2\27\u015b\3\2\2\2\31\u015e\3\2\2\2\33\u0165\3\2\2\2\35\u016a\3"+
		"\2\2\2\37\u016f\3\2\2\2!\u0177\3\2\2\2#\u017d\3\2\2\2%\u0185\3\2\2\2\'"+
		"\u018b\3\2\2\2)\u018f\3\2\2\2+\u0192\3\2\2\2-\u0197\3\2\2\2/\u01a2\3\2"+
		"\2\2\61\u01ad\3\2\2\2\63\u01b7\3\2\2\2\65\u01bc\3\2\2\2\67\u01c3\3\2\2"+
		"\29\u01c7\3\2\2\2;\u01cf\3\2\2\2=\u01d7\3\2\2\2?\u01e1\3\2\2\2A\u01e8"+
		"\3\2\2\2C\u01ef\3\2\2\2E\u01f5\3\2\2\2G\u01fc\3\2\2\2I\u0204\3\2\2\2K"+
		"\u020a\3\2\2\2M\u0217\3\2\2\2O\u021c\3\2\2\2Q\u0222\3\2\2\2S\u022c\3\2"+
		"\2\2U\u0230\3\2\2\2W\u0235\3\2\2\2Y\u023e\3\2\2\2[\u0244\3\2\2\2]\u024b"+
		"\3\2\2\2_\u0256\3\2\2\2a\u025d\3\2\2\2c\u0264\3\2\2\2e\u026b\3\2\2\2g"+
		"\u0272\3\2\2\2i\u0279\3\2\2\2k\u0282\3\2\2\2m\u0288\3\2\2\2o\u0293\3\2"+
		"\2\2q\u029c\3\2\2\2s\u02a0\3\2\2\2u\u02a4\3\2\2\2w\u02a9\3\2\2\2y\u02ae"+
		"\3\2\2\2{\u02b7\3\2\2\2}\u02bf\3\2\2\2\177\u02c2\3\2\2\2\u0081\u02ca\3"+
		"\2\2\2\u0083\u02d1\3\2\2\2\u0085\u02d8\3\2\2\2\u0087\u02dd\3\2\2\2\u0089"+
		"\u02ea\3\2\2\2\u008b\u02ef\3\2\2\2\u008d\u02f7\3\2\2\2\u008f\u02ff\3\2"+
		"\2\2\u0091\u030b\3\2\2\2\u0093\u031a\3\2\2\2\u0095\u0327\3\2\2\2\u0097"+
		"\u0335\3\2\2\2\u0099\u0340\3\2\2\2\u009b\u0342\3\2\2\2\u009d\u0349\3\2"+
		"\2\2\u009f\u034f\3\2\2\2\u00a1\u0351\3\2\2\2\u00a3\u0354\3\2\2\2\u00a5"+
		"\u0359\3\2\2\2\u00a7\u035b\3\2\2\2\u00a9\u035d\3\2\2\2\u00ab\u035f\3\2"+
		"\2\2\u00ad\u0361\3\2\2\2\u00af\u0363\3\2\2\2\u00b1\u0365\3\2\2\2\u00b3"+
		"\u0367\3\2\2\2\u00b5\u0369\3\2\2\2\u00b7\u036b\3\2\2\2\u00b9\u036d\3\2"+
		"\2\2\u00bb\u0370\3\2\2\2\u00bd\u0373\3\2\2\2\u00bf\u0375\3\2\2\2\u00c1"+
		"\u0377\3\2\2\2\u00c3\u0379\3\2\2\2\u00c5\u037b\3\2\2\2\u00c7\u037d\3\2"+
		"\2\2\u00c9\u037f\3\2\2\2\u00cb\u0382\3\2\2\2\u00cd\u0386\3\2\2\2\u00cf"+
		"\u0389\3\2\2\2\u00d1\u038c\3\2\2\2\u00d3\u0390\3\2\2\2\u00d5\u0393\3\2"+
		"\2\2\u00d7\u0396\3\2\2\2\u00d9\u0399\3\2\2\2\u00db\u039c\3\2\2\2\u00dd"+
		"\u039e\3\2\2\2\u00df\u03a0\3\2\2\2\u00e1\u03a2\3\2\2\2\u00e3\u03a4\3\2"+
		"\2\2\u00e5\u03a6\3\2\2\2\u00e7\u03a8\3\2\2\2\u00e9\u03aa\3\2\2\2\u00eb"+
		"\u03ac\3\2\2\2\u00ed\u03af\3\2\2\2\u00ef\u03b2\3\2\2\2\u00f1\u03b5\3\2"+
		"\2\2\u00f3\u03b8\3\2\2\2\u00f5\u03bb\3\2\2\2\u00f7\u03be\3\2\2\2\u00f9"+
		"\u03c1\3\2\2\2\u00fb\u03c4\3\2\2\2\u00fd\u03c7\3\2\2\2\u00ff\u03cb\3\2"+
		"\2\2\u0101\u03cf\3\2\2\2\u0103\u03d4\3\2\2\2\u0105\u03e1\3\2\2\2\u0107"+
		"\u03e9\3\2\2\2\u0109\u03eb\3\2\2\2\u010b\u03ed\3\2\2\2\u010d\u03f2\3\2"+
		"\2\2\u010f\u03f8\3\2\2\2\u0111\u0402\3\2\2\2\u0113\u0412\3\2\2\2\u0115"+
		"\u0420\3\2\2\2\u0117\u0118\7c\2\2\u0118\u0119\7d\2\2\u0119\u011a\7u\2"+
		"\2\u011a\u011b\7v\2\2\u011b\u011c\7t\2\2\u011c\u011d\7c\2\2\u011d\u011e"+
		"\7e\2\2\u011e\u011f\7v\2\2\u011f\4\3\2\2\2\u0120\u0121\7d\2\2\u0121\u0122"+
		"\7q\2\2\u0122\u0123\7q\2\2\u0123\u0124\7n\2\2\u0124\u0125\7g\2\2\u0125"+
		"\u0126\7c\2\2\u0126\u0127\7p\2\2\u0127\6\3\2\2\2\u0128\u0129\7d\2\2\u0129"+
		"\u012a\7t\2\2\u012a\u012b\7g\2\2\u012b\u012c\7c\2\2\u012c\u012d\7m\2\2"+
		"\u012d\b\3\2\2\2\u012e\u012f\7d\2\2\u012f\u0130\7{\2\2\u0130\u0131\7v"+
		"\2\2\u0131\u0132\7g\2\2\u0132\n\3\2\2\2\u0133\u0134\7e\2\2\u0134\u0135"+
		"\7c\2\2\u0135\u0136\7v\2\2\u0136\u0137\7e\2\2\u0137\u0138\7j\2\2\u0138"+
		"\f\3\2\2\2\u0139\u013a\7e\2\2\u013a\u013b\7j\2\2\u013b\u013c\7c\2\2\u013c"+
		"\u013d\7t\2\2\u013d\16\3\2\2\2\u013e\u013f\7e\2\2\u013f\u0140\7n\2\2\u0140"+
		"\u0141\7c\2\2\u0141\u0142\7u\2\2\u0142\u0143\7u\2\2\u0143\20\3\2\2\2\u0144"+
		"\u0145\7e\2\2\u0145\u0146\7q\2\2\u0146\u0147\7p\2\2\u0147\u0148\7u\2\2"+
		"\u0148\u0149\7v\2\2\u0149\22\3\2\2\2\u014a\u014b\7e\2\2\u014b\u014c\7"+
		"q\2\2\u014c\u014d\7p\2\2\u014d\u014e\7v\2\2\u014e\u014f\7k\2\2\u014f\u0150"+
		"\7p\2\2\u0150\u0151\7w\2\2\u0151\u0152\7g\2\2\u0152\24\3\2\2\2\u0153\u0154"+
		"\7f\2\2\u0154\u0155\7g\2\2\u0155\u0156\7h\2\2\u0156\u0157\7c\2\2\u0157"+
		"\u0158\7w\2\2\u0158\u0159\7n\2\2\u0159\u015a\7v\2\2\u015a\26\3\2\2\2\u015b"+
		"\u015c\7f\2\2\u015c\u015d\7q\2\2\u015d\30\3\2\2\2\u015e\u015f\7f\2\2\u015f"+
		"\u0160\7q\2\2\u0160\u0161\7w\2\2\u0161\u0162\7d\2\2\u0162\u0163\7n\2\2"+
		"\u0163\u0164\7g\2\2\u0164\32\3\2\2\2\u0165\u0166\7g\2\2\u0166\u0167\7"+
		"n\2\2\u0167\u0168\7u\2\2\u0168\u0169\7g\2\2\u0169\34\3\2\2\2\u016a\u016b"+
		"\7g\2\2\u016b\u016c\7p\2\2\u016c\u016d\7w\2\2\u016d\u016e\7o\2\2\u016e"+
		"\36\3\2\2\2\u016f\u0170\7g\2\2\u0170\u0171\7z\2\2\u0171\u0172\7v\2\2\u0172"+
		"\u0173\7g\2\2\u0173\u0174\7p\2\2\u0174\u0175\7f\2\2\u0175\u0176\7u\2\2"+
		"\u0176 \3\2\2\2\u0177\u0178\7h\2\2\u0178\u0179\7k\2\2\u0179\u017a\7p\2"+
		"\2\u017a\u017b\7c\2\2\u017b\u017c\7n\2\2\u017c\"\3\2\2\2\u017d\u017e\7"+
		"h\2\2\u017e\u017f\7k\2\2\u017f\u0180\7p\2\2\u0180\u0181\7c\2\2\u0181\u0182"+
		"\7n\2\2\u0182\u0183\7n\2\2\u0183\u0184\7{\2\2\u0184$\3\2\2\2\u0185\u0186"+
		"\7h\2\2\u0186\u0187\7n\2\2\u0187\u0188\7q\2\2\u0188\u0189\7c\2\2\u0189"+
		"\u018a\7v\2\2\u018a&\3\2\2\2\u018b\u018c\7h\2\2\u018c\u018d\7q\2\2\u018d"+
		"\u018e\7t\2\2\u018e(\3\2\2\2\u018f\u0190\7k\2\2\u0190\u0191\7h\2\2\u0191"+
		"*\3\2\2\2\u0192\u0193\7i\2\2\u0193\u0194\7q\2\2\u0194\u0195\7v\2\2\u0195"+
		"\u0196\7q\2\2\u0196,\3\2\2\2\u0197\u0198\7k\2\2\u0198\u0199\7o\2\2\u0199"+
		"\u019a\7r\2\2\u019a\u019b\7n\2\2\u019b\u019c\7g\2\2\u019c\u019d\7o\2\2"+
		"\u019d\u019e\7g\2\2\u019e\u019f\7p\2\2\u019f\u01a0\7v\2\2\u01a0\u01a1"+
		"\7u\2\2\u01a1.\3\2\2\2\u01a2\u01a3\7k\2\2\u01a3\u01a4\7p\2\2\u01a4\u01a5"+
		"\7u\2\2\u01a5\u01a6\7v\2\2\u01a6\u01a7\7c\2\2\u01a7\u01a8\7p\2\2\u01a8"+
		"\u01a9\7e\2\2\u01a9\u01aa\7g\2\2\u01aa\u01ab\7q\2\2\u01ab\u01ac\7h\2\2"+
		"\u01ac\60\3\2\2\2\u01ad\u01ae\7k\2\2\u01ae\u01af\7p\2\2\u01af\u01b0\7"+
		"v\2\2\u01b0\u01b1\7g\2\2\u01b1\u01b2\7t\2\2\u01b2\u01b3\7h\2\2\u01b3\u01b4"+
		"\7c\2\2\u01b4\u01b5\7e\2\2\u01b5\u01b6\7g\2\2\u01b6\62\3\2\2\2\u01b7\u01b8"+
		"\7n\2\2\u01b8\u01b9\7q\2\2\u01b9\u01ba\7p\2\2\u01ba\u01bb\7i\2\2\u01bb"+
		"\64\3\2\2\2\u01bc\u01bd\7p\2\2\u01bd\u01be\7c\2\2\u01be\u01bf\7v\2\2\u01bf"+
		"\u01c0\7k\2\2\u01c0\u01c1\7x\2\2\u01c1\u01c2\7g\2\2\u01c2\66\3\2\2\2\u01c3"+
		"\u01c4\7p\2\2\u01c4\u01c5\7g\2\2\u01c5\u01c6\7y\2\2\u01c68\3\2\2\2\u01c7"+
		"\u01c8\7r\2\2\u01c8\u01c9\7c\2\2\u01c9\u01ca\7e\2\2\u01ca\u01cb\7m\2\2"+
		"\u01cb\u01cc\7c\2\2\u01cc\u01cd\7i\2\2\u01cd\u01ce\7g\2\2\u01ce:\3\2\2"+
		"\2\u01cf\u01d0\7r\2\2\u01d0\u01d1\7t\2\2\u01d1\u01d2\7k\2\2\u01d2\u01d3"+
		"\7x\2\2\u01d3\u01d4\7c\2\2\u01d4\u01d5\7v\2\2\u01d5\u01d6\7g\2\2\u01d6"+
		"<\3\2\2\2\u01d7\u01d8\7r\2\2\u01d8\u01d9\7t\2\2\u01d9\u01da\7q\2\2\u01da"+
		"\u01db\7v\2\2\u01db\u01dc\7g\2\2\u01dc\u01dd\7e\2\2\u01dd\u01de\7v\2\2"+
		"\u01de\u01df\7g\2\2\u01df\u01e0\7f\2\2\u01e0>\3\2\2\2\u01e1\u01e2\7r\2"+
		"\2\u01e2\u01e3\7w\2\2\u01e3\u01e4\7d\2\2\u01e4\u01e5\7n\2\2\u01e5\u01e6"+
		"\7k\2\2\u01e6\u01e7\7e\2\2\u01e7@\3\2\2\2\u01e8\u01e9\7t\2\2\u01e9\u01ea"+
		"\7g\2\2\u01ea\u01eb\7v\2\2\u01eb\u01ec\7w\2\2\u01ec\u01ed\7t\2\2\u01ed"+
		"\u01ee\7p\2\2\u01eeB\3\2\2\2\u01ef\u01f0\7u\2\2\u01f0\u01f1\7j\2\2\u01f1"+
		"\u01f2\7q\2\2\u01f2\u01f3\7t\2\2\u01f3\u01f4\7v\2\2\u01f4D\3\2\2\2\u01f5"+
		"\u01f6\7u\2\2\u01f6\u01f7\7v\2\2\u01f7\u01f8\7c\2\2\u01f8\u01f9\7v\2\2"+
		"\u01f9\u01fa\7k\2\2\u01fa\u01fb\7e\2\2\u01fbF\3\2\2\2\u01fc\u01fd\7x\2"+
		"\2\u01fd\u01fe\7k\2\2\u01fe\u01ff\7t\2\2\u01ff\u0200\7v\2\2\u0200\u0201"+
		"\7w\2\2\u0201\u0202\7c\2\2\u0202\u0203\7n\2\2\u0203H\3\2\2\2\u0204\u0205"+
		"\7u\2\2\u0205\u0206\7w\2\2\u0206\u0207\7r\2\2\u0207\u0208\7g\2\2\u0208"+
		"\u0209\7t\2\2\u0209J\3\2\2\2\u020a\u020b\7u\2\2\u020b\u020c\7{\2\2\u020c"+
		"\u020d\7p\2\2\u020d\u020e\7e\2\2\u020e\u020f\7j\2\2\u020f\u0210\7t\2\2"+
		"\u0210\u0211\7q\2\2\u0211\u0212\7p\2\2\u0212\u0213\7k\2\2\u0213\u0214"+
		"\7|\2\2\u0214\u0215\7g\2\2\u0215\u0216\7f\2\2\u0216L\3\2\2\2\u0217\u0218"+
		"\7v\2\2\u0218\u0219\7j\2\2\u0219\u021a\7k\2\2\u021a\u021b\7u\2\2\u021b"+
		"N\3\2\2\2\u021c\u021d\7v\2\2\u021d\u021e\7j\2\2\u021e\u021f\7t\2\2\u021f"+
		"\u0220\7q\2\2\u0220\u0221\7y\2\2\u0221P\3\2\2\2\u0222\u0223\7v\2\2\u0223"+
		"\u0224\7t\2\2\u0224\u0225\7c\2\2\u0225\u0226\7p\2\2\u0226\u0227\7u\2\2"+
		"\u0227\u0228\7k\2\2\u0228\u0229\7g\2\2\u0229\u022a\7p\2\2\u022a\u022b"+
		"\7v\2\2\u022bR\3\2\2\2\u022c\u022d\7v\2\2\u022d\u022e\7t\2\2\u022e\u022f"+
		"\7{\2\2\u022fT\3\2\2\2\u0230\u0231\7x\2\2\u0231\u0232\7q\2\2\u0232\u0233"+
		"\7k\2\2\u0233\u0234\7f\2\2\u0234V\3\2\2\2\u0235\u0236\7x\2\2\u0236\u0237"+
		"\7q\2\2\u0237\u0238\7n\2\2\u0238\u0239\7c\2\2\u0239\u023a\7v\2\2\u023a"+
		"\u023b\7k\2\2\u023b\u023c\7n\2\2\u023c\u023d\7g\2\2\u023dX\3\2\2\2\u023e"+
		"\u023f\7y\2\2\u023f\u0240\7j\2\2\u0240\u0241\7k\2\2\u0241\u0242\7n\2\2"+
		"\u0242\u0243\7g\2\2\u0243Z\3\2\2\2\u0244\u0245\7i\2\2\u0245\u0246\7n\2"+
		"\2\u0246\u0247\7q\2\2\u0247\u0248\7d\2\2\u0248\u0249\7c\2\2\u0249\u024a"+
		"\7n\2\2\u024a\\\3\2\2\2\u024b\u024c\7y\2\2\u024c\u024d\7g\2\2\u024d\u024e"+
		"\7d\2\2\u024e\u024f\7u\2\2\u024f\u0250\7g\2\2\u0250\u0251\7t\2\2\u0251"+
		"\u0252\7x\2\2\u0252\u0253\7k\2\2\u0253\u0254\7e\2\2\u0254\u0255\7g\2\2"+
		"\u0255^\3\2\2\2\u0256\u0257\7u\2\2\u0257\u0258\7g\2\2\u0258\u0259\7n\2"+
		"\2\u0259\u025a\7g\2\2\u025a\u025b\7e\2\2\u025b\u025c\7v\2\2\u025c`\3\2"+
		"\2\2\u025d\u025e\7k\2\2\u025e\u025f\7p\2\2\u025f\u0260\7u\2\2\u0260\u0261"+
		"\7g\2\2\u0261\u0262\7t\2\2\u0262\u0263\7v\2\2\u0263b\3\2\2\2\u0264\u0265"+
		"\7w\2\2\u0265\u0266\7r\2\2\u0266\u0267\7u\2\2\u0267\u0268\7g\2\2\u0268"+
		"\u0269\7t\2\2\u0269\u026a\7v\2\2\u026ad\3\2\2\2\u026b\u026c\7w\2\2\u026c"+
		"\u026d\7r\2\2\u026d\u026e\7f\2\2\u026e\u026f\7c\2\2\u026f\u0270\7v\2\2"+
		"\u0270\u0271\7g\2\2\u0271f\3\2\2\2\u0272\u0273\7f\2\2\u0273\u0274\7g\2"+
		"\2\u0274\u0275\7n\2\2\u0275\u0276\7g\2\2\u0276\u0277\7v\2\2\u0277\u0278"+
		"\7g\2\2\u0278h\3\2\2\2\u0279\u027a\7w\2\2\u027a\u027b\7p\2\2\u027b\u027c"+
		"\7f\2\2\u027c\u027d\7g\2\2\u027d\u027e\7n\2\2\u027e\u027f\7g\2\2\u027f"+
		"\u0280\7v\2\2\u0280\u0281\7g\2\2\u0281j\3\2\2\2\u0282\u0283\7o\2\2\u0283"+
		"\u0284\7g\2\2\u0284\u0285\7t\2\2\u0285\u0286\7i\2\2\u0286\u0287\7g\2\2"+
		"\u0287l\3\2\2\2\u0288\u0289\7v\2\2\u0289\u028a\7g\2\2\u028a\u028b\7u\2"+
		"\2\u028b\u028c\7v\2\2\u028c\u028d\7o\2\2\u028d\u028e\7g\2\2\u028e\u028f"+
		"\7v\2\2\u028f\u0290\7j\2\2\u0290\u0291\7q\2\2\u0291\u0292\7f\2\2\u0292"+
		"n\3\2\2\2\u0293\u0294\7q\2\2\u0294\u0295\7x\2\2\u0295\u0296\7g\2\2\u0296"+
		"\u0297\7t\2\2\u0297\u0298\7t\2\2\u0298\u0299\7k\2\2\u0299\u029a\7f\2\2"+
		"\u029a\u029b\7g\2\2\u029bp\3\2\2\2\u029c\u029d\7i\2\2\u029d\u029e\7g\2"+
		"\2\u029e\u029f\7v\2\2\u029fr\3\2\2\2\u02a0\u02a1\7u\2\2\u02a1\u02a2\7"+
		"g\2\2\u02a2\u02a3\7v\2\2\u02a3t\3\2\2\2\u02a4\u02a5\7d\2\2\u02a5\u02a6"+
		"\7n\2\2\u02a6\u02a7\7q\2\2\u02a7\u02a8\7d\2\2\u02a8v\3\2\2\2\u02a9\u02aa"+
		"\7f\2\2\u02aa\u02ab\7c\2\2\u02ab\u02ac\7v\2\2\u02ac\u02ad\7g\2\2\u02ad"+
		"x\3\2\2\2\u02ae\u02af\7f\2\2\u02af\u02b0\7c\2\2\u02b0\u02b1\7v\2\2\u02b1"+
		"\u02b2\7g\2\2\u02b2\u02b3\7v\2\2\u02b3\u02b4\7k\2\2\u02b4\u02b5\7o\2\2"+
		"\u02b5\u02b6\7g\2\2\u02b6z\3\2\2\2\u02b7\u02b8\7f\2\2\u02b8\u02b9\7g\2"+
		"\2\u02b9\u02ba\7e\2\2\u02ba\u02bb\7k\2\2\u02bb\u02bc\7o\2\2\u02bc\u02bd"+
		"\7c\2\2\u02bd\u02be\7n\2\2\u02be|\3\2\2\2\u02bf\u02c0\7k\2\2\u02c0\u02c1"+
		"\7f\2\2\u02c1~\3\2\2\2\u02c2\u02c3\7k\2\2\u02c3\u02c4\7p\2\2\u02c4\u02c5"+
		"\7v\2\2\u02c5\u02c6\7g\2\2\u02c6\u02c7\7i\2\2\u02c7\u02c8\7g\2\2\u02c8"+
		"\u02c9\7t\2\2\u02c9\u0080\3\2\2\2\u02ca\u02cb\7q\2\2\u02cb\u02cc\7d\2"+
		"\2\u02cc\u02cd\7l\2\2\u02cd\u02ce\7g\2\2\u02ce\u02cf\7e\2\2\u02cf\u02d0"+
		"\7v\2\2\u02d0\u0082\3\2\2\2\u02d1\u02d2\7u\2\2\u02d2\u02d3\7v\2\2\u02d3"+
		"\u02d4\7t\2\2\u02d4\u02d5\7k\2\2\u02d5\u02d6\7p\2\2\u02d6\u02d7\7i\2\2"+
		"\u02d7\u0084\3\2\2\2\u02d8\u02d9\7v\2\2\u02d9\u02da\7k\2\2\u02da\u02db"+
		"\7o\2\2\u02db\u02dc\7g\2\2\u02dc\u0086\3\2\2\2\u02dd\u02de\7u\2\2\u02de"+
		"\u02df\7{\2\2\u02df\u02e0\7u\2\2\u02e0\u02e1\7v\2\2\u02e1\u02e2\7g\2\2"+
		"\u02e2\u02e3\7o\2\2\u02e3\u02e4\7\60\2\2\u02e4\u02e5\7t\2\2\u02e5\u02e6"+
		"\7w\2\2\u02e6\u02e7\7p\2\2\u02e7\u02e8\7c\2\2\u02e8\u02e9\7u\2\2\u02e9"+
		"\u0088\3\2\2\2\u02ea\u02eb\7y\2\2\u02eb\u02ec\7k\2\2\u02ec\u02ed\7v\2"+
		"\2\u02ed\u02ee\7j\2\2\u02ee\u008a\3\2\2\2\u02ef\u02f0\7y\2\2\u02f0\u02f1"+
		"\7k\2\2\u02f1\u02f2\7v\2\2\u02f2\u02f3\7j\2\2\u02f3\u02f4\7q\2\2\u02f4"+
		"\u02f5\7w\2\2\u02f5\u02f6\7v\2\2\u02f6\u008c\3\2\2\2\u02f7\u02f8\7u\2"+
		"\2\u02f8\u02f9\7j\2\2\u02f9\u02fa\7c\2\2\u02fa\u02fb\7t\2\2\u02fb\u02fc"+
		"\7k\2\2\u02fc\u02fd\7p\2\2\u02fd\u02fe\7i\2\2\u02fe\u008e\3\2\2\2\u02ff"+
		"\u0300\7y\2\2\u0300\u0301\7k\2\2\u0301\u0302\7v\2\2\u0302\u0303\7j\2\2"+
		"\u0303\u0304\7u\2\2\u0304\u0305\7j\2\2\u0305\u0306\7c\2\2\u0306\u0307"+
		"\7t\2\2\u0307\u0308\7k\2\2\u0308\u0309\7p\2\2\u0309\u030a\7i\2\2\u030a"+
		"\u0090\3\2\2\2\u030b\u030c\7y\2\2\u030c\u030d\7k\2\2\u030d\u030e\7v\2"+
		"\2\u030e\u030f\7j\2\2\u030f\u0310\7q\2\2\u0310\u0311\7w\2\2\u0311\u0312"+
		"\7v\2\2\u0312\u0313\7u\2\2\u0313\u0314\7j\2\2\u0314\u0315\7c\2\2\u0315"+
		"\u0316\7t\2\2\u0316\u0317\7k\2\2\u0317\u0318\7p\2\2\u0318\u0319\7i\2\2"+
		"\u0319\u0092\3\2\2\2\u031a\u031e\5\u0097L\2\u031b\u031d\5\u0097L\2\u031c"+
		"\u031b\3\2\2\2\u031d\u0320\3\2\2\2\u031e\u031c\3\2\2\2\u031e\u031f\3\2"+
		"\2\2\u031f\u0322\3\2\2\2\u0320\u031e\3\2\2\2\u0321\u0323\t\2\2\2\u0322"+
		"\u0321\3\2\2\2\u0322\u0323\3\2\2\2\u0323\u0094\3\2\2\2\u0324\u0326\5\u0097"+
		"L\2\u0325\u0324\3\2\2\2\u0326\u0329\3\2\2\2\u0327\u0325\3\2\2\2\u0327"+
		"\u0328\3\2\2\2\u0328\u032a\3\2\2\2\u0329\u0327\3\2\2\2\u032a\u032b\7\60"+
		"\2\2\u032b\u032f\5\u0097L\2\u032c\u032e\5\u0097L\2\u032d\u032c\3\2\2\2"+
		"\u032e\u0331\3\2\2\2\u032f\u032d\3\2\2\2\u032f\u0330\3\2\2\2\u0330\u0333"+
		"\3\2\2\2\u0331\u032f\3\2\2\2\u0332\u0334\t\3\2\2\u0333\u0332\3\2\2\2\u0333"+
		"\u0334\3\2\2\2\u0334\u0096\3\2\2\2\u0335\u0336\t\4\2\2\u0336\u0098\3\2"+
		"\2\2\u0337\u0338\7v\2\2\u0338\u0339\7t\2\2\u0339\u033a\7w\2\2\u033a\u0341"+
		"\7g\2\2\u033b\u033c\7h\2\2\u033c\u033d\7c\2\2\u033d\u033e\7n\2\2\u033e"+
		"\u033f\7u\2\2\u033f\u0341\7g\2\2\u0340\u0337\3\2\2\2\u0340\u033b\3\2\2"+
		"\2\u0341\u009a\3\2\2\2\u0342\u0344\7)\2\2\u0343\u0345\5\u009dO\2\u0344"+
		"\u0343\3\2\2\2\u0344\u0345\3\2\2\2\u0345\u0346\3\2\2\2\u0346\u0347\7)"+
		"\2\2\u0347\u009c\3\2\2\2\u0348\u034a\5\u009fP\2\u0349\u0348\3\2\2\2\u034a"+
		"\u034b\3\2\2\2\u034b\u0349\3\2\2\2\u034b\u034c\3\2\2\2\u034c\u009e\3\2"+
		"\2\2\u034d\u0350\n\5\2\2\u034e\u0350\5\u00a1Q\2\u034f\u034d\3\2\2\2\u034f"+
		"\u034e\3\2\2\2\u0350\u00a0\3\2\2\2\u0351\u0352\7^\2\2\u0352\u0353\t\6"+
		"\2\2\u0353\u00a2\3\2\2\2\u0354\u0355\7p\2\2\u0355\u0356\7w\2\2\u0356\u0357"+
		"\7n\2\2\u0357\u0358\7n\2\2\u0358\u00a4\3\2\2\2\u0359\u035a\7*\2\2\u035a"+
		"\u00a6\3\2\2\2\u035b\u035c\7+\2\2\u035c\u00a8\3\2\2\2\u035d\u035e\7}\2"+
		"\2\u035e\u00aa\3\2\2\2\u035f\u0360\7\177\2\2\u0360\u00ac\3\2\2\2\u0361"+
		"\u0362\7]\2\2\u0362\u00ae\3\2\2\2\u0363\u0364\7_\2\2\u0364\u00b0\3\2\2"+
		"\2\u0365\u0366\7=\2\2\u0366\u00b2\3\2\2\2\u0367\u0368\7.\2\2\u0368\u00b4"+
		"\3\2\2\2\u0369\u036a\7\60\2\2\u036a\u00b6\3\2\2\2\u036b\u036c\7?\2\2\u036c"+
		"\u00b8\3\2\2\2\u036d\u036e\7>\2\2\u036e\u036f\7?\2\2\u036f\u00ba\3\2\2"+
		"\2\u0370\u0371\7@\2\2\u0371\u0372\7?\2\2\u0372\u00bc\3\2\2\2\u0373\u0374"+
		"\7@\2\2\u0374\u00be\3\2\2\2\u0375\u0376\7>\2\2\u0376\u00c0\3\2\2\2\u0377"+
		"\u0378\7#\2\2\u0378\u00c2\3\2\2\2\u0379\u037a\7\u0080\2\2\u037a\u00c4"+
		"\3\2\2\2\u037b\u037c\7A\2\2\u037c\u00c6\3\2\2\2\u037d\u037e\7<\2\2\u037e"+
		"\u00c8\3\2\2\2\u037f\u0380\7?\2\2\u0380\u0381\7?\2\2\u0381\u00ca\3\2\2"+
		"\2\u0382\u0383\7?\2\2\u0383\u0384\7?\2\2\u0384\u0385\7?\2\2\u0385\u00cc"+
		"\3\2\2\2\u0386\u0387\7#\2\2\u0387\u0388\7?\2\2\u0388\u00ce\3\2\2\2\u0389"+
		"\u038a\7>\2\2\u038a\u038b\7@\2\2\u038b\u00d0\3\2\2\2\u038c\u038d\7#\2"+
		"\2\u038d\u038e\7?\2\2\u038e\u038f\7?\2\2\u038f\u00d2\3\2\2\2\u0390\u0391"+
		"\7(\2\2\u0391\u0392\7(\2\2\u0392\u00d4\3\2\2\2\u0393\u0394\7~\2\2\u0394"+
		"\u0395\7~\2\2\u0395\u00d6\3\2\2\2\u0396\u0397\7-\2\2\u0397\u0398\7-\2"+
		"\2\u0398\u00d8\3\2\2\2\u0399\u039a\7/\2\2\u039a\u039b\7/\2\2\u039b\u00da"+
		"\3\2\2\2\u039c\u039d\7-\2\2\u039d\u00dc\3\2\2\2\u039e\u039f\7/\2\2\u039f"+
		"\u00de\3\2\2\2\u03a0\u03a1\7,\2\2\u03a1\u00e0\3\2\2\2\u03a2\u03a3\7\61"+
		"\2\2\u03a3\u00e2\3\2\2\2\u03a4\u03a5\7(\2\2\u03a5\u00e4\3\2\2\2\u03a6"+
		"\u03a7\7~\2\2\u03a7\u00e6\3\2\2\2\u03a8\u03a9\7`\2\2\u03a9\u00e8\3\2\2"+
		"\2\u03aa\u03ab\7\'\2\2\u03ab\u00ea\3\2\2\2\u03ac\u03ad\7?\2\2\u03ad\u03ae"+
		"\7@\2\2\u03ae\u00ec\3\2\2\2\u03af\u03b0\7-\2\2\u03b0\u03b1\7?\2\2\u03b1"+
		"\u00ee\3\2\2\2\u03b2\u03b3\7/\2\2\u03b3\u03b4\7?\2\2\u03b4\u00f0\3\2\2"+
		"\2\u03b5\u03b6\7,\2\2\u03b6\u03b7\7?\2\2\u03b7\u00f2\3\2\2\2\u03b8\u03b9"+
		"\7\61\2\2\u03b9\u03ba\7?\2\2\u03ba\u00f4\3\2\2\2\u03bb\u03bc\7(\2\2\u03bc"+
		"\u03bd\7?\2\2\u03bd\u00f6\3\2\2\2\u03be\u03bf\7~\2\2\u03bf\u03c0\7?\2"+
		"\2\u03c0\u00f8\3\2\2\2\u03c1\u03c2\7`\2\2\u03c2\u03c3\7?\2\2\u03c3\u00fa"+
		"\3\2\2\2\u03c4\u03c5\7\'\2\2\u03c5\u03c6\7?\2\2\u03c6\u00fc\3\2\2\2\u03c7"+
		"\u03c8\7>\2\2\u03c8\u03c9\7>\2\2\u03c9\u03ca\7?\2\2\u03ca\u00fe\3\2\2"+
		"\2\u03cb\u03cc\7@\2\2\u03cc\u03cd\7@\2\2\u03cd\u03ce\7?\2\2\u03ce\u0100"+
		"\3\2\2\2\u03cf\u03d0\7@\2\2\u03d0\u03d1\7@\2\2\u03d1\u03d2\7@\2\2\u03d2"+
		"\u03d3\7?\2\2\u03d3\u0102\3\2\2\2\u03d4\u03d8\5\u0105\u0083\2\u03d5\u03d7"+
		"\5\u0107\u0084\2\u03d6\u03d5\3\2\2\2\u03d7\u03da\3\2\2\2\u03d8\u03d6\3"+
		"\2\2\2\u03d8\u03d9\3\2\2\2\u03d9\u0104\3\2\2\2\u03da\u03d8\3\2\2\2\u03db"+
		"\u03e2\t\7\2\2\u03dc\u03dd\n\b\2\2\u03dd\u03e2\6\u0083\2\2\u03de\u03df"+
		"\t\t\2\2\u03df\u03e0\t\n\2\2\u03e0\u03e2\6\u0083\3\2\u03e1\u03db\3\2\2"+
		"\2\u03e1\u03dc\3\2\2\2\u03e1\u03de\3\2\2\2\u03e2\u0106\3\2\2\2\u03e3\u03ea"+
		"\t\13\2\2\u03e4\u03e5\n\b\2\2\u03e5\u03ea\6\u0084\4\2\u03e6\u03e7\t\t"+
		"\2\2\u03e7\u03e8\t\n\2\2\u03e8\u03ea\6\u0084\5\2\u03e9\u03e3\3\2\2\2\u03e9"+
		"\u03e4\3\2\2\2\u03e9\u03e6\3\2\2\2\u03ea\u0108\3\2\2\2\u03eb\u03ec\7B"+
		"\2\2\u03ec\u010a\3\2\2\2\u03ed\u03ee\7\60\2\2\u03ee\u03ef\7\60\2\2\u03ef"+
		"\u03f0\7\60\2\2\u03f0\u010c\3\2\2\2\u03f1\u03f3\t\f\2\2\u03f2\u03f1\3"+
		"\2\2\2\u03f3\u03f4\3\2\2\2\u03f4\u03f2\3\2\2\2\u03f4\u03f5\3\2\2\2\u03f5"+
		"\u03f6\3\2\2\2\u03f6\u03f7\b\u0087\2\2\u03f7\u010e\3\2\2\2\u03f8\u03f9"+
		"\7\61\2\2\u03f9\u03fa\7\61\2\2\u03fa\u03fb\7#\2\2\u03fb\u03ff\3\2\2\2"+
		"\u03fc\u03fe\n\r\2\2\u03fd\u03fc\3\2\2\2\u03fe\u0401\3\2\2\2\u03ff\u03fd"+
		"\3\2\2\2\u03ff\u0400\3\2\2\2\u0400\u0110\3\2\2\2\u0401\u03ff\3\2\2\2\u0402"+
		"\u0403\7\61\2\2\u0403\u0404\7,\2\2\u0404\u0405\7,\2\2\u0405\u0406\3\2"+
		"\2\2\u0406\u040a\t\r\2\2\u0407\u0409\13\2\2\2\u0408\u0407\3\2\2\2\u0409"+
		"\u040c\3\2\2\2\u040a\u040b\3\2\2\2\u040a\u0408\3\2\2\2\u040b\u040d\3\2"+
		"\2\2\u040c\u040a\3\2\2\2\u040d\u040e\7,\2\2\u040e\u040f\7\61\2\2\u040f"+
		"\u0410\3\2\2\2\u0410\u0411\b\u0089\3\2\u0411\u0112\3\2\2\2\u0412\u0413"+
		"\7\61\2\2\u0413\u0414\7,\2\2\u0414\u0418\3\2\2\2\u0415\u0417\13\2\2\2"+
		"\u0416\u0415\3\2\2\2\u0417\u041a\3\2\2\2\u0418\u0419\3\2\2\2\u0418\u0416"+
		"\3\2\2\2\u0419\u041b\3\2\2\2\u041a\u0418\3\2\2\2\u041b\u041c\7,\2\2\u041c"+
		"\u041d\7\61\2\2\u041d\u041e\3\2\2\2\u041e\u041f\b\u008a\3\2\u041f\u0114"+
		"\3\2\2\2\u0420\u0421\7\61\2\2\u0421\u0422\7\61\2\2\u0422\u0426\3\2\2\2"+
		"\u0423\u0425\n\r\2\2\u0424\u0423\3\2\2\2\u0425\u0428\3\2\2\2\u0426\u0424"+
		"\3\2\2\2\u0426\u0427\3\2\2\2\u0427\u0429\3\2\2\2\u0428\u0426\3\2\2\2\u0429"+
		"\u042a\b\u008b\3\2\u042a\u0116\3\2\2\2\24\2\u031e\u0322\u0327\u032f\u0333"+
		"\u0340\u0344\u034b\u034f\u03d8\u03e1\u03e9\u03f4\u03ff\u040a\u0418\u0426"+
		"\4\2\4\2\2\5\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}