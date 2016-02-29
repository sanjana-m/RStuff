import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.text.ParseException;
import java.text.SimpleDateFormat;

public class Transaction implements Comparable<Transaction> {
	public double amount;
	public Date date;
	public String type;
	public double units;
	public String name;
	
	public Transaction(String name, double amount, String date1, String type, double units){
		this.amount = amount;
		this.type = type;
		this.units = units;
		this.name = name;
		String delim = null;
		
		Pattern pattern = Pattern.compile("[^0-9]");
		Matcher matcher = pattern.matcher(date1.toString());
		if(matcher.find()){
			delim = matcher.group(0);
		}		
		SimpleDateFormat fmt = new SimpleDateFormat("dd"+delim.charAt(0)+"MM"+delim.charAt(0)+"yy");
		try {
			this.date = fmt.parse(date1);
//			date.setYear(date.getYear()+2000);
		} 
		catch (ParseException e) {
			e.printStackTrace();
		}
	}
	
	// GETTER METHODS	
	public double getAmount(){
		return this.amount;
	}
	
	public String getType(){
		return this.type;
	}
	
	public Date getDate(){
		return this.date;
	}
	
	public double getUnits(){
		return this.units;
	}
	
	public String getName(){
		return this.name;
	}
	
	// SETTER METHODS
	public void setAmount(double amt){
		this.amount = amt;
	}

	public void setName(String name){
		this.name = name;
	}

	public void setDate(String date1){
		String delim = null;
		
		Pattern pattern = Pattern.compile("[^0-9]");
		Matcher matcher = pattern.matcher(date1.toString());
		if(matcher.find()){
			delim = matcher.group(0);
		}		
		SimpleDateFormat fmt = new SimpleDateFormat("dd"+delim.charAt(0)+"MM"+delim.charAt(0)+"yy");
		try {
			this.date = fmt.parse(date1);
		} 
		catch (ParseException e) {
			e.printStackTrace();
		}
	}
	
	public void setUnits(double units){
		this.units = units;
	}
	public void setType(String type){
		this.type = type;
	}
	
	public int compareTo(Transaction other) {
        return date.compareTo(other.date);
    }

}
