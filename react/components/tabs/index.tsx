import React, { useCallback, useState, useLayoutEffect } from 'react';
import './index.less';
import Tab from './tab';

export const prefix = 'c7n-custom-tabs';
export interface ITab {
  key: string
  title: React.ReactNode
}

export interface TabsProps {
  defaultActiveKey?: string
  activeKey?: string
  onChange?: (activeKey: string) => void
  tabs: ITab[]
  type?: 'button' | 'line'
  color?: string
}
const Tabs: React.FC<TabsProps> = ({
  tabs, defaultActiveKey, activeKey: propsActiveKey, onChange, type = 'line', color = '#5365EA',
}) => {
  const [activeKey, setActiveKey] = useState<string | null>(propsActiveKey ?? defaultActiveKey ?? tabs[0]?.key);
  useLayoutEffect(() => {
    if (!activeKey && tabs.length > 0) {
      setActiveKey(tabs[0].key);
    }
  }, [activeKey, tabs]);
  useLayoutEffect(() => {
    if (propsActiveKey) {
      setActiveKey(propsActiveKey);
    }
  }, [propsActiveKey]);
  const handleChange = useCallback((nextActiveKey) => {
    onChange && onChange(nextActiveKey);
  }, [onChange]);
  return (
    <div className={prefix}>
      {tabs.map((tab) => (
        <Tab
          type={type}
          color={color}
          active={tab.key === activeKey}
          key={tab.key}
          data={tab}
          onChange={handleChange}
        />
      ))}
    </div>
  );
};
export default Tabs;
