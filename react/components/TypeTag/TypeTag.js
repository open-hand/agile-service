import React, { memo } from 'react';
import { Icon } from 'choerodon-ui';
import './TypeTag.less';

const TypeTag = ({
  data, showName, style, featureType, iconSize,
}) => {
  let { colour, name = '', icon } = data || {};
  if (featureType === 'business') {
    colour = '#3D5AFE';
    name = '特性';
    icon = 'characteristic';
  } else if (featureType === 'enabler') {
    colour = '#FFCA28';
    name = '使能';
  }
  return (
    <div className="c7n-typeTag" style={style}>
      {icon === 'characteristic' ? (
        <Icon
          className="c7n-typeTag-icon-normal"
          style={{
            fontSize: iconSize * 15 / 24 || '15px',
            background: colour || '#fab614',
            color: 'white',
          }}
          type={icon}
        />
      ) : (
        <Icon
          style={{
            fontSize: iconSize || '26px',
            color: colour || '#fab614',
          }}
          type={icon || 'help'}
        />
      )}
      {
        showName && (
          <span className="name">{name}</span>
        )
      }
    </div>
  );
};
export default memo(TypeTag);
