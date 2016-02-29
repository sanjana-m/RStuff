import java.io.BufferedReader;
import java.io.FileReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class ProcessTransaction {
	
	public List<Transaction> trans;
	public List<List<Transaction>> funds;

	public ProcessTransaction(){
		this.trans = new ArrayList<Transaction>();
	}

	// Pass the filename and the columns for processing the Investment data
	public void readData(String file, int namecol, int typecol, int datecol, int amtcol, int unitcol){
		String filename = file.replace("\\","\\\\");		
		
		BufferedReader br = null;
		String line = "";
		String cvsSplitBy = ",";
		
		String date, type, name, amt1 = null, amt2 = null;
		Double units, amount;
		int count = 0;
		
		try {
			br = new BufferedReader(new FileReader(filename));

			while ((line = br.readLine()) != null) {
				if(count == 0) {
					count++;
					continue;
				}
				Pattern pattern = Pattern.compile("(?<=\")(.*)(?=\")");
				Matcher matcher = pattern.matcher(line.toString());
				if(matcher.find()){
					amt2 = matcher.group(0);
					amt1 = amt2.replace(",","");
				}
				
				line = line.replace(("\""+amt2+"\""),"");
				String[] flow = line.split(cvsSplitBy);
				amount = new Double(amt1);
				date = new String(flow[datecol]);
				units = new Double(flow[unitcol]);
				name = new String(flow[namecol]);
				type = new String(flow[typecol]);
				Transaction t = new Transaction(name,amount,date,type,units);
//				System.out.println("DATE1 - "+flow[datecol]+" dayt: "+t.getDate());
				trans.add(t);
			}			
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}
	
	public List<String> fundNames(){
		List<String> funds = new ArrayList<String>();
		for(int i=0; i< trans.size(); i++){
			if(!funds.contains(trans.get(i).getName())){
				funds.add(trans.get(i).getName());
			}
		}
		return funds;
	}
	
	public void getFunds(List<String> fundnames){
		funds = new ArrayList<List<Transaction>>(fundnames.size());
		int fundcount=0;
		
		for(String fund1 : fundnames){
			List<Transaction> trans1 = new ArrayList<Transaction>();
			for(int i=0; i<this.trans.size(); i++){
				if(this.trans.get(i).getName().equals(fund1)){
					System.out.println(trans.get(i).getName()+" DATE - "+ trans.get(i).getDate());
					trans1.add(this.trans.get(i));
				}
			}
			funds.add(trans1);
			fundcount++;
		}
	}
	
	public static void main(String[] args){
		ProcessTransaction p = new ProcessTransaction();
		p.readData(args[0], 0, 1, 2, 3, 5);
		SimpleDateFormat fmt = new SimpleDateFormat("dd-MM-yyyy");
		Collections.sort(p.trans);
		List<String> fundnames = p.fundNames();
		p.getFunds(fundnames);
		
		for(int i=0; i<p.funds.size(); i++){
			List<Transaction> trans1 = p.funds.get(i);
			for(int j=0; j < trans1.size(); j++){
				System.out.println(trans1.get(j).getName()+" Date 2: "+ p.trans.get(j).getDate()+" Units: "+trans1.get(j).getUnits());				
			}
		}
	}
}
