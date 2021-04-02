import React, { useCallback } from 'react';
import classNames from 'classnames';
import { prefix, ITab, TabsProps } from '../index';
import './index.less';

export type TabProps = {
  active: boolean
  onChange: (activeKey: string) => void
  data: ITab
} & Required<Pick<TabsProps, 'type' | 'color'>>
const Tab: React.FC<TabProps> = ({
  onChange, data, active, type, color,
}) => {
  const prefixCls = `${prefix}-${type}`;
  const handleClick = useCallback(() => {
    if (!active) {
      onChange(data.key);
    }
  }, [active, data.key, onChange]);
  return (
    <div
      role="none"
      style={{ color }}
      className={classNames(`${prefixCls}-tab`, {
        [`${prefixCls}-tab-active`]: active,
      })}
      onClick={handleClick}
    >
      <div className={`${prefixCls}-tab-title`}>
        {data.title}
      </div>
      <div className={classNames(`${prefixCls}-tab-indicator`)} />
    </div>
  );
};

export default Tab;
