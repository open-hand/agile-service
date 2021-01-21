import React, { useMemo } from 'react';
import { toJS } from 'mobx';
import { User } from '@/common/types';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import HeadTag, { HeadTagProps } from '../head-tag';
import { getFistStr } from './util';
import styles from './index.less';
/**
 * 数组内只有一个User 或者User是对象时,显示名字
 */
interface Props extends HeadTagProps {
  data: User[] | User /**   */
  maxTagCount?: number /** @default 3 */
  style?: React.CSSProperties
}
export const UserUniqueTag: React.FC<{ data: User } & HeadTagProps> = ({
  data, size = 18, showText = true, ...otherProps
}) => {
  const { realName, email, loginName } = data;
  return (
    <HeadTag
      size={size}
      src={data.imageUrl!}
      name={getFistStr(realName)}
      text={realName || loginName}
      showText={showText}
      tooltip={data.ldap ? `${realName}(${loginName})` : `${realName}(${email})`}
      avatarStyle={{
        color: '#6473c3',
        borderRadius: '50%',
        backgroundColor: '#c5cbe8',
      }}
      {...otherProps}
    />
  );
};
const UserTag: React.FC<Props> = ({
  data: propsData, style, maxTagCount = 3, ...otherProps
}) => {
  // return <span>1</span>;
  const data = useMemo(() => {
    const newData = toJS(propsData);
    if (!newData) {
      return [];
    }
    if (Array.isArray(newData) && newData.length === 1) {
      return newData[0];
    }
    return newData;
  }, [propsData]);
  const compact = maxTagCount && Array.isArray(data) && data.length > maxTagCount;
  return Array.isArray(data) ? (
    <div style={{ display: 'inline-flex', ...style }}>
      {data.slice(0, maxTagCount).map((item) => (
        <UserUniqueTag
          className={compact ? styles.compact : undefined}
          avatarClassName={compact ? styles.avatar : undefined}
          key={item.id}
          data={item}
          showText={false}
          size={compact ? 22 : 18}
          {...otherProps}
        />
      ))}
      {maxTagCount && data.length > maxTagCount ? (
        <Tooltip
          // @ts-ignore
          popupCls={styles.tooltip}
          theme="light"
          title={(
            <div>
              {data.slice(maxTagCount).map((item) => (
                <div key={item.id} style={{ marginBottom: 5 }}>
                  <UserUniqueTag
                    tooltip={false}
                    data={item}
                    textStyle={{ color: '#000', lineHeight: '18px' }}
                    {...otherProps}
                  />
                </div>
              ))}
            </div>
          )}
        >
          <HeadTag
            className={compact ? styles.compact : undefined}
            avatarClassName={compact ? styles.avatar : undefined}
            name={<Icon type="more_horiz" style={{ fontSize: 'inherit', lineHeight: 'inherit' }} />}
            size={compact ? 24 : 18}
            avatarStyle={{ backgroundColor: 'rgb(240, 245, 255)', borderRadius: '50%' }}
          />
        </Tooltip>
      ) : null}
    </div>
  ) : <UserUniqueTag data={data} {...otherProps} />;
};
export default UserTag;
