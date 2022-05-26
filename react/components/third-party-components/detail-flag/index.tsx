import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import './index.less';
import classNames from 'classnames';
import { observer } from 'mobx-react';

interface IIssueDetailHeaderFlagProps {
  data?: { instanceOpenRelVO?: { source: 'yqcloud' | string, openInstanceNum: string, openInstanceId: string } }
  hidden?: boolean
  className?: string
  style?: React.CSSProperties
}
const IssueDetailHeaderFlag: React.FC<IIssueDetailHeaderFlagProps> = observer(({
  data, hidden, className, style,
}) => {
  const prefix = 'c7n-agile-third-components-detail-flag';
  if (hidden || !data?.instanceOpenRelVO?.openInstanceId) {
    return <></>;
  }
  const source = data.instanceOpenRelVO.source === 'yqcloud' ? '来自燕千云的单据：' : '来自三方应用平台：';
  return (
    <Tooltip title={`${source}:${data.instanceOpenRelVO.openInstanceNum}`}>
      <div className={classNames(prefix, className)} style={style}>
        <svg width="20px" height="28px" viewBox="0 0 54 38" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlnsXlink="http://www.w3.org/1999/xlink">
          <g id="页面-1" stroke="none" strokeWidth="1" fill="none" fillRule="evenodd">
            <g id="燕千云logo" transform="translate(-0.000000, 0.000000)" fillRule="nonzero">
              <path d="M53.3561451,4.48836363 C52.8409582,3.86386371 52.2725678,3.2853068 51.6573763,2.7592099 C51.3645761,2.49521696 51.0374839,2.2470636 50.6840134,1.99363038 C48.8058715,0.649906339 46.8591457,-0.0206357179 44.909782,0.000483716961 C41.9105582,0.0321628693 39.1065344,1.68739858 36.611138,4.91603218 C36.3684567,5.22754385 36.1204998,5.56017495 35.8619915,5.91392548 C35.2500126,6.74814316 34.5905527,7.6958778 33.8625089,8.74392975 L32.9366271,10.0638944 C29.8731919,14.5388693 26.4859111,18.7829419 22.8020465,22.7619547 C22.0986226,23.5116946 21.3644238,24.2702343 20.5994503,25.0375738 C18.8347354,26.7984066 16.9064745,28.5961985 14.7539972,30.4045501 L14.7170675,30.4335894 L14.7038783,30.4335894 C14.0860454,30.9647661 13.9719194,31.8784976 14.4400943,32.5455329 C14.4585592,32.5719322 14.4822997,32.6088912 14.5139538,32.6484901 C15.1602246,33.5091071 16.2285497,34.1057311 17.0858476,34.5861983 C17.2414802,34.670676 17.3865614,34.7525138 17.5131777,34.8264318 L11.6492598,34.8264318 C8.42947322,34.8309847 5.47810639,33.0312561 4.00584161,30.165514 C2.53357682,27.2997719 2.78821583,23.8503587 4.66530299,21.2322609 C6.54239014,18.614163 9.7259867,17.2680412 12.9101472,17.7460889 C13.2057224,17.7891515 13.4989051,17.8473058 13.7885479,17.9203242 L13.901975,17.9520034 C14.0173237,17.9764558 14.1348997,17.988842 14.2528077,17.9889624 C15.1269109,17.9889624 15.8355116,17.2798004 15.8355116,16.4050048 C15.8395203,16.3408291 15.8395203,16.2764656 15.8355116,16.2122899 C15.8355116,16.1885305 15.8355116,16.1621313 15.8355116,16.135732 C15.7855203,15.8367801 15.7511816,15.5354168 15.7326358,15.2328761 C15.7326358,15.1299189 15.7326358,15.0269616 15.7326358,14.9213645 C15.7326358,10.1799721 19.5732514,6.33631418 24.3108909,6.33631418 C24.8202296,6.33597053 25.3286256,6.38013709 25.8302866,6.46831065 L25.9146975,6.48415022 C25.9950972,6.49564463 26.076167,6.50181787 26.1573788,6.50262973 C26.9752021,6.50908592 27.6632523,5.89089012 27.744699,5.07646231 C27.8261458,4.26203449 27.274162,3.5196327 26.4712817,3.36375372 L26.4053357,3.36375372 C25.714535,3.23532407 25.0135084,3.16993734 24.3108909,3.16839895 C17.9725165,3.16498702 12.7750894,8.19595047 12.567228,14.5359348 C12.2612386,14.5121754 11.9552492,14.5016157 11.6439841,14.5016157 C5.29387669,14.5570014 0.137522839,19.6532431 0.00269752023,26.0071895 C-0.132127799,32.3611359 4.80339916,37.671858 11.1454324,37.996987 C11.205156,38.0010043 11.2650818,38.0010043 11.3248055,37.996987 L29.4230245,37.996987 C29.6868084,37.996987 29.9505924,37.996987 30.2143764,37.9811474 C32.4305608,37.9049469 34.6229907,37.4983232 36.7192894,36.7746997 C37.6074338,36.4657892 38.4729841,36.0953013 39.3096481,35.6659294 C40.4607403,35.0738646 41.5487245,34.3662473 42.5568289,33.5539859 C45.3819554,31.270447 47.5687246,27.9863749 48.911385,24.0502402 C49.634825,21.8684352 50.1160522,19.6135518 50.3463699,17.3263401 C50.4334186,16.507962 50.4914511,15.6790242 50.5231052,14.8448065 C50.5442079,14.2851415 50.5231052,13.7281164 50.5231052,13.181651 C50.5231052,13.0813337 50.5231052,12.9810164 50.5231052,12.8806991 C50.4756241,11.2624224 50.4281429,9.73390326 51.1060678,8.54065519 C51.5208327,7.82589477 52.135077,7.24786426 52.8734205,6.8774997 L52.9288151,6.84582055 C53.3261641,6.6225345 53.6064648,6.23693624 53.6964265,5.7898488 C53.7818644,5.32696972 53.6571681,4.85003998 53.3561451,4.48836363 Z M45.7631017,23.0594333 C44.6306758,26.4372015 42.793105,29.2365303 40.4496124,31.1522795 C38.995555,32.3295111 37.3513324,33.2433593 35.5896178,33.8534327 C33.8019649,34.4766164 31.9324481,34.8262924 30.0428279,34.8909091 C29.8226339,34.8909091 29.60244,34.8909091 29.3822461,34.8909091 L29.1201104,34.8909091 C25.63172,34.8793034 22.2065773,33.9473924 19.1825486,32.1871024 C19.0147818,32.0889269 18.8417723,31.9907514 18.6582774,31.8872691 L18.2519671,31.6537706 C20.0213827,30.0856158 21.6466236,28.530728 23.1460394,27.0050275 C23.9411841,26.1966273 24.7005037,25.4006097 25.423998,24.6169745 C28.6314593,21.1026535 31.60867,17.3799767 34.3366095,13.4727273 C36.9796395,17.7296592 41.9941452,19.8278931 46.8352364,18.7025633 C46.5980256,20.1824741 46.2394068,21.6398054 45.7631017,23.0594333 Z M47.2548943,13.0338196 C47.2730986,13.651762 47.2913029,14.2377418 47.270498,14.7970861 C47.270498,14.9808707 47.270498,15.1646553 47.2470924,15.3457763 C43.0036468,16.9611266 38.2703569,14.8710845 36.5039343,10.6020035 L36.5273399,10.5673774 C37.5259776,9.09976436 38.3893832,7.83191716 39.1409622,6.83841505 C41.000405,4.38795407 42.9352657,3.1334246 44.893532,3.1094527 C46.8517983,3.08548079 48.5187952,4.25744039 49.5902503,5.24028833 C49.0556582,5.74414983 48.5995934,6.32892294 48.2379283,6.97425582 C47.1352658,8.96126005 47.1976806,11.1240582 47.2548943,13.0338196 Z" id="形状" fill="#2979FF" />
              <path d="M37.3516485,24.2392929 C37.4264231,24.0531715 37.4649983,23.8544148 37.4652928,23.6537475 C37.4652928,23.3803354 37.3920097,23.1119514 37.2535012,22.8764747 C37.2061239,22.7935192 37.1507592,22.7154189 37.0882003,22.6432929 C37.056433,22.6045601 37.0219046,22.5681918 36.9848873,22.5344747 C36.9406043,22.4904139 36.893122,22.4497112 36.8428319,22.412702 C36.8035954,22.3812683 36.7621641,22.3526952 36.7188563,22.327202 C36.6555599,22.2878885 36.5890244,22.2540832 36.5199787,22.2261566 C36.4814164,22.2087902 36.4417215,22.1940744 36.4011687,22.1821111 L36.3030214,22.1510202 L36.2281194,22.1354747 C36.1252638,22.1145217 36.0205523,22.1041046 35.9155975,22.1043838 L35.9155975,22.1043838 C35.863941,22.1043838 35.8097017,22.1043838 35.7580452,22.1043838 C35.7068713,22.109424 35.656003,22.1172079 35.6056585,22.127702 C35.5731966,22.1327148 35.5412242,22.1405162 35.5100939,22.1510202 C35.2504322,22.2219247 35.0136524,22.3594367 34.8230623,22.5500202 L34.8230623,22.5500202 L34.7765715,22.5992475 L34.7765715,22.5992475 C32.81284,24.6612247 29.9421758,25.5860784 27.1487853,25.0567227 C24.3553948,24.527367 22.0189028,22.6157418 20.9403751,19.9772475 L20.9403751,19.9565202 C20.9409997,19.9478952 20.9409997,19.9392361 20.9403751,19.9306111 C20.8681054,19.7790151 20.7721974,19.6399505 20.6562642,19.5186566 C20.6175219,19.477202 20.5787795,19.4357475 20.5374543,19.3968838 L20.4263928,19.3087929 C20.3754946,19.2689766 20.3219966,19.2326226 20.2662576,19.1999747 L20.1681102,19.1481566 C20.1255408,19.1269166 20.0815222,19.1087346 20.0363861,19.0937475 C19.9751762,19.0696676 19.9121209,19.0506051 19.8478398,19.0367475 C19.7740667,19.0184439 19.698907,19.0063114 19.623134,19.0004747 L19.5043241,19.0004747 C19.3992873,19.0011542 19.2945856,19.0124383 19.1918022,19.0341566 L19.0445811,19.0704293 C19.0024375,19.0836909 18.9610503,19.0992597 18.9206055,19.1170656 C18.8904198,19.1278345 18.861068,19.1408243 18.8327894,19.1559293 L18.7656359,19.1896111 C18.690766,19.2307631 18.6191037,19.2775327 18.5512614,19.3295202 C18.5021877,19.3709747 18.4531141,19.4124293 18.409206,19.4564747 L18.3575495,19.5108838 C18.2976089,19.5789584 18.2431958,19.6517362 18.1948315,19.7285202 C18.1767517,19.7570202 18.1586719,19.7855202 18.143175,19.8140202 C18.0939215,19.9025414 18.0549256,19.9964245 18.0269478,20.0938384 C18.0030636,20.1672794 17.9857759,20.2427164 17.9752913,20.3192475 C17.9627516,20.3971988 17.9567041,20.4760621 17.9571558,20.5550202 L17.9571558,20.6301566 C17.9647615,20.7766694 17.9925778,20.9214182 18.039862,21.0602475 L18.0631074,21.1250202 L18.0786044,21.1612929 L18.0786044,21.1612929 C19.5519818,24.7830837 22.7569578,27.4073099 26.589397,28.1299085 C30.4218362,28.852507 34.3579455,27.5747177 37.0417095,24.7367475 C37.1327621,24.6421373 37.2109261,24.5358501 37.2741638,24.4206566 C37.2877417,24.3990599 37.2998234,24.3765522 37.3103233,24.3532929 L37.3516485,24.2392929 Z" id="路径" fill="#FF9100" />
            </g>
          </g>
        </svg>
      </div>
    </Tooltip>
  );
});
IssueDetailHeaderFlag.defaultProps = {
  data: undefined,
  hidden: false,
  className: undefined,
  style: undefined,
};
export default IssueDetailHeaderFlag;
