import React, { useCallback } from 'react';
import { Tooltip } from 'choerodon-ui';
import classNames from 'classnames';
import { User } from '@/common/types';
import styles from './index.less';

function getFirst(str: string): string {
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
interface UserTagProps {
  data: User
  title?: string
  tooltip?: boolean
  hiddenText?: boolean
  size?: number
  className?: string
  style?: React.CSSProperties
}
const UserTag: React.FC<UserTagProps> = ({
  data,
  size,
  hiddenText,
  style,
  className,
  tooltip = true,
  title,
  ...otherProps
}) => {
  const iconSize = size || 18;
  const {
    id, loginName, realName, avatar, imageUrl, email, ldap, name,
  } = data || {};
  const img = avatar || imageUrl;
  const renderTooltip = useCallback(() => {
    if (title) {
      return title;
    }
    if (name) {
      return name;
    }
    return ldap ? `${realName}(${loginName})` : `${realName}(${email})`;
  }, [email, ldap, loginName, name, realName, title]);

  const renderContent = useCallback(() => (
    <div
      className={classNames(styles.user_tag, className)}
      style={style}
      {...otherProps}
    >
      <div
        className={styles.user_tag_avatar}
        style={{
          width: iconSize,
          height: iconSize,
          lineHeight: iconSize,
          backgroundImage: img ? `url(${img})` : undefined,
        }}
      >
        {!img && (
          <span style={{
            color: '#6473c3',
          }}
          >
            {getFirst(realName)}
          </span>
        )}
      </div>
      {
        !hiddenText && (
          <span
            className={styles.user_tag_text}
          >
            {`${realName || loginName}`}
          </span>
        )
      }
    </div>
  ), [className, hiddenText, iconSize, img, loginName, realName, style]);
  if (!(id || loginName)) {
    return null;
  }
  return tooltip
    ? (
      <Tooltip title={renderTooltip()} mouseEnterDelay={0.5}>
        {renderContent()}
      </Tooltip>
    )
    : renderContent();
};
export default UserTag;
