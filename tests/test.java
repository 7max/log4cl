// Copyright (c) 2012, Max Mikhanosha. All rights reserved.
// 
// This file is licensed to You under the Apache License, Version 2.0
// (the "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//
// Used to test log4j vs log4cl speed
//

import org.apache.log4j.*;

public class test
{
    public static void main(String args[])
    {
	System.out.println("Hello there");
	PropertyConfigurator.configure("log4j.properties");
	int limit = 1;

        if (args.length > 0)
            limit = Integer.valueOf(args[0]).intValue();
	
	final Category log = Category.getInstance("cat1.logger");
	
	for (int i = 0; i < limit; i++)
	    log.debug("iter=" + i);
    }
}
