import React from 'react';
import { Tooltip } from 'choerodon-ui';

function getFirst(str: string) {
  if (!str) {
    return '';
  }
  const re = /[\u4E00-\u9FA5]/g;
  for (let i = 0, len = str.length; i < len; i += 1) {
    if (re.test(str[i])) {
      return str[i];
    }
  }
  return str[0];
}
interface UserHeadProps {
  user: {
    id?: number | string,
    loginName?: string,
    realName: string,
    avatar?: string,
    imageUrl?: string | null,
    email?: string,
    ldap?: boolean,
    name?: string,
  }
  color?: string
  size?: number
  hiddenText?: boolean
  style?: React.CSSProperties
  className?: string
  type?: 'datalog'
  tooltip?: boolean
  title?: string
}
const UserHead: React.FC<UserHeadProps> = ({
  user,
  color,
  size,
  hiddenText,
  style,
  className = '',
  type,
  tooltip = true,
  title,
  ...restProps
}) => {
  const iconSize = size || 18;
  const {
    id, loginName, realName, avatar, imageUrl, email, ldap, name,
  } = user || {};
  const img = avatar || imageUrl;
  const renderTooltip = () => {
    if (title) {
      return title;
    }
    if (name) {
      return name;
    }
    return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
  };

  const renderContent = () => (
    <div
      className={`c7n-userHead ${className}`}
      style={{
        display: (id || loginName) ? 'flex' : 'none',
        maxWidth: 108,
        ...style,
      }}
      {...restProps}
    >
      {
        type === 'datalog' ? (
          <div
            style={{
              width: 40,
              height: 40,
              background: '#b3bac5',
              color: '#fff',
              overflow: 'hidden',
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
              textAlign: 'center',
              borderRadius: 4,
              flexShrink: 0,
            }}
          >
            {
              img ? (
                <img src={img} alt="" style={{ width: '100%' }} />
              ) : (
                <span
                  style={{
                    width: 40, height: 40, lineHeight: '40px', textAlign: 'center', color: '#fff', fontSize: '12px',
                  }}
                  className="user-Head-Title"
                >
                  {getFirst(realName)}
                </span>
              )
            }
          </div>
        ) : (
          <div
            style={{
              width: iconSize,
              height: iconSize,
              background: '#c5cbe8',
              color: '#6473c3',
              overflow: 'hidden',
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
              marginRight: 5,
              textAlign: 'center',
              borderRadius: '50%',
              flexShrink: 0,
            }}
          >
            {
                img ? (
                  <img src={img} alt="" style={{ width: iconSize, height: iconSize }} />
                ) : (
                  <span style={{
                    width: iconSize, height: iconSize, lineHeight: `${iconSize}px`, textAlign: 'center', color: '#6473c3',
                  }}
                  >
                    {getFirst(realName)}
                  </span>
                )
              }
          </div>
        )
      }
      {
        hiddenText ? null : (
          <span
            style={{
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
              // fontSize: '13px',
              lineHeight: `${iconSize}px`,
              color: color || 'rgba(0, 0, 0, 0.65)',
            }}
          >
            {`${realName || loginName}`}
          </span>
        )
      }
    </div>
  );
  return tooltip
    ? (
      <Tooltip title={renderTooltip()} mouseEnterDelay={0.5}>
        {renderContent()}
      </Tooltip>
    )
    : renderContent();
};
export default UserHead;
