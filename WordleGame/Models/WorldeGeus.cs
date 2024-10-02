using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WordleGame.Models
{
    public class WorldeGeus
    {
        public string Word { get; set; }
        public int[] statusArray = new int[5];
        //public string[] CorrectPosition = new string[5];
        //public string[] WrongPosition = new string[5];
        //public string[] NotInWord = new string[5];
    }
}
