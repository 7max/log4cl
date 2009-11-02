import org.apache.log4j.*;

public class test
{
    public static void main(String args[])
    {
	System.out.println("Hello there");
	PropertyConfigurator.configure("/home/max/cl-log/log4j.properties");
	int limit = 1;

        if (args.length > 0)
            limit = Integer.valueOf(args[0]).intValue();
	
	final Category log = Category.getInstance("cat1.logger");
	
	for (int i = 0; i < limit; i++)
	    log.debug("iter=" + i);
    }
}
