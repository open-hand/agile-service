import React, { useMemo } from 'react';
import { toJS } from 'mobx';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import { User } from '@/common/types';
import HeadTag, { HeadTagProps } from '../head-tag';
import { getFistStr } from './util';
import './index.less';
/**
 * 数组内只有一个User 或者User是对象时,显示名字
 */
type UserTagData = Pick<User, 'loginName' | 'realName' | 'imageUrl' | 'textShow'> & Partial<Pick<User, 'ldap' | 'email' | 'id' | 'textShow'>> & Pick<HeadTagProps, 'tooltip'>
interface Props extends HeadTagProps {
  data: UserTagData[] | UserTagData /**   */
  maxTagCount?: number /** @default 3 */
  style?: React.CSSProperties
}
export const UserUniqueTag: React.FC<{ data: UserTagData, prefixCls?: string } & HeadTagProps> = ({
  data, size = 18, showText = true, prefixCls = 'c7n-agile-user-tag', style, avatarStyle, ...otherProps
}) => {
  const {
    realName, email, loginName, textShow,
  } = data;
  const text = useMemo(() => textShow || realName || loginName, [loginName, realName, textShow]);
  return text ? (
    <HeadTag
      size={size}
      src={data.imageUrl!}
      name={getFistStr(realName)}
      text={text}
      showText={showText}
      textClassName={`${prefixCls}-text`}
      tooltip={data.tooltip ?? (data.ldap ? `${realName}(${loginName})` : `${realName}(${email})`)}
      style={{ maxWidth: 108, ...style }}
      avatarStyle={{
        color: '#6473c3',
        borderRadius: '50%',
        backgroundColor: '#c5cbe8',
        ...avatarStyle,
      }}
      {...otherProps}
    />
  ) : <span className={`${prefixCls}-no-user`} />;
};
const UserTag: React.FC<Props> = ({
  data: propsData, style, maxTagCount = 3, ...otherProps
}) => {
  // return <span>1</span>;
  const prefixCls = 'c7n-agile-user-tag';
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
          className={compact ? `${prefixCls}-compact` : undefined}
          avatarClassName={compact ? `${prefixCls}-compact-avatar` : undefined}
          key={item.realName}
          data={item}
          showText={false}
          size={compact ? 22 : 18}
          {...otherProps}
        />
      ))}
      {maxTagCount && data.length > maxTagCount ? (
        <Tooltip
          // @ts-ignore
          popupCls={`${prefixCls}-tooltip`}
          theme="light"
          title={(
            <div className={`${prefixCls}-tooltip-content`}>
              {data.slice(maxTagCount).map((item) => (
                <div key={item.realName} style={{ marginBottom: 5 }}>
                  <UserUniqueTag
                    tooltip={false}
                    data={item}
                    className={`${prefixCls}-tooltip-user`}
                    textStyle={{ color: 'var(--text-color)', lineHeight: '18px' }}
                    {...otherProps}
                  />
                </div>
              ))}
            </div>
          )}
        >
          <HeadTag
            className={compact ? `${prefixCls}-compact` : undefined}
            avatarClassName={compact ? `${prefixCls}-compact` : undefined}
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
