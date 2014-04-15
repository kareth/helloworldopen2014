using System;
using System.IO;
using System.Net.Sockets;
using Newtonsoft.Json;

public class Bot {
	public static void Main(string[] args) {
	    string host = args[0];
        int port = int.Parse(args[1]);
        string botName = args[2];
        string botKey = args[3];

		Console.WriteLine("Connecting to " + host + ":" + port + " as " + botName + "/" + botKey);

		using(TcpClient client = new TcpClient(host, port)) {
			NetworkStream stream = client.GetStream();
			StreamReader reader = new StreamReader(stream);
			StreamWriter writer = new StreamWriter(stream);
			writer.AutoFlush = true;

			new Bot(reader, writer, new Join(botName, botKey));
		}
	}

	private StreamWriter writer;

	Bot(StreamReader reader, StreamWriter writer, Join join) {
		this.writer = writer;
		string line;

		send(join);

		while((line = reader.ReadLine()) != null) {
			MsgWrapper msg = JsonConvert.DeserializeObject<MsgWrapper>(line);
			switch(msg.msgType) {
				case "carPositions":
					send(new Throttle(0.5));
					break;
				case "join":
					Console.WriteLine("Joined");
					send(new Ping());
					break;
				case "gameInit":
					Console.WriteLine("Race init");
					send(new Ping());
					break;
				case "gameEnd":
					Console.WriteLine("Race ended");
					send(new Ping());
					break;
				case "gameStart":
					Console.WriteLine("Race starts");
					send(new Ping());
					break;
				default:
					send(new Ping());
					break;
			}
		}
	}

	private void send(SendMsg msg) {
		writer.WriteLine(msg.ToJson());
	}
}

class MsgWrapper {
    public string msgType;
    public Object data;

    public MsgWrapper(string msgType, Object data) {
    	this.msgType = msgType;
    	this.data = data;
    }
}

abstract class SendMsg {
	public string ToJson() {
		return JsonConvert.SerializeObject(new MsgWrapper(this.MsgType(), this.MsgData()));
	}
	protected virtual Object MsgData() {
        return this;
    }

    protected abstract string MsgType();
}

class Join: SendMsg {
	public string name;
	public string key;
	public string color;

	public Join(string name, string key) {
		this.name = name;
		this.key = key;
		this.color = "red";
	}

	protected override string MsgType() { 
		return "join";
	}
}

class Ping: SendMsg {
	protected override string MsgType() {
		return "ping";
	}
}

class Throttle: SendMsg {
	public double value;

	public Throttle(double value) {
		this.value = value;
	}

	protected override Object MsgData() {
		return this.value;
	}

	protected override string MsgType() {
		return "throttle";
	}
}