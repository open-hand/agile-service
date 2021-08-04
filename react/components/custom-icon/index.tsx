import { IconProps } from 'choerodon-ui/lib/icon';
import { Icon } from 'choerodon-ui/lib';
import classnames from 'classnames';
import React from 'react';

const IconPom: React.FC<Pick<IconProps, 'height' | 'width' | 'className'> & { multicolor?: boolean }> = ({
  height = '1em', width = '1em', className, multicolor,
}) => (
  <svg className={classnames('c7nicon', className)} fill="currentcolor" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="1082" width={height} height={width}>
    <defs>
      <linearGradient id="orange_red" x1="0%" y1="0%" x2="100%" y2="0%">
        <stop
          offset="0%"
          stopColor="#FDC403"
          stopOpacity={1}

        />
        <stop
          offset="40%"
          stopColor="#F39513"
          stopOpacity={1}
        />
        <stop
          offset="70%"
          stopColor="#E76025"
          stopOpacity={1}
        />
        <stop
          offset="100%"
          stopColor="#AD2279"
          stopOpacity={1}
        />
      </linearGradient>
    </defs>
    <path d="M389.888 51.2L153.6 287.488h236.288z" p-id="1083" />
    <path d="M390.0416 464.7424V353.4336l92.16 62.3616c124.16 84.0192 179.6608 102.6048 224.256 117.6064 12.9024 4.4032 25.2416 8.6528 37.888 13.824V51.2H449.024v295.3728H153.6v531.712h461.1072a428.4928 428.4928 0 0 1-33.4848-24.064C455.68 755.0464 390.0416 583.168 390.0416 464.7424z" p-id="1084" />
    <path d="M887.7568 869.7856s-129.6384 48.9984-269.9776-61.952c-110.4384-87.1936-168.8064-241.8176-168.8064-343.04 249.4976 168.704 245.1456 90.3168 371.2 202.4448 101.2736 90.0608 67.584 202.5472 67.584 202.5472" p-id="1085" fill={multicolor ? 'url(#orange_red)' : undefined} />
    <path d="M921.6 937.216s-11.5712 3.9936-23.552-5.632c-16.2816-12.9024-58.7264-106.3424-179.9168-183.9616-110.2848-70.5024-161.6384-131.584-249.3952-243.3536 130.56 153.7024 143.0528 140.4416 283.0848 234.7008 71.168 47.9232 143.2064 121.7536 169.7792 164.5568v33.6896z" p-id="1086" />
  </svg>
);
const IconExpand: React.FC<Pick<IconProps, 'height' | 'width' | 'className'|'style'>> = ({
  height = '1em', width = '1em', className, style,
}) => <svg className={classnames('c7nicon', 'icon', className)} style={style} fill="currentcolor" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="1088" width={height} height={width}><path d="M128 896h768v-85.333333H128v85.333333zM128 341.333333v341.333334l170.666667-170.666667-170.666667-170.666667z m341.333333 384h426.666667v-85.333333H469.333333v85.333333zM128 128v85.333333h768V128H128z m341.333333 256h426.666667V298.666667H469.333333v85.333333z m0 170.666667h426.666667v-85.333334H469.333333v85.333334z" p-id="1089" /></svg>;
interface ICustomIconProps extends Omit<IconProps, 'scriptUrl'> {
}
// const iconList = { 'icon-pom': true };
type ICustomIonType = 'icon-pom' | 'icon-pom-multiColor' | 'icon-indent'
const CustomIcon: React.FC<ICustomIconProps & { type: ICustomIonType | string }> = ({ type, ...otherProps }) => {
  switch (type) {
    case 'icon-pom':
    case 'icon-pom-multiColor':
      return <IconPom {...otherProps} multicolor={type === 'icon-pom-multiColor'} />;
    case 'icon-indent':
      return <IconExpand {...otherProps} />;
    default:
      break;
  }
  return <Icon type={type} {...otherProps} />;
};
// <Icon scriptUrl="//at.alicdn.com/t/font_891039_5orlxrdz7g3.js" {...props} />
export default CustomIcon;
