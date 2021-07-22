/* eslint-disable jsx-a11y/click-events-have-key-events, jsx-a11y/no-static-element-interactions */

import React, {
  FC, memo, useEffect, useState,
} from 'react';
import './index.less';

interface Props {
  onChange?(value: string): void,
  defaultActiveKey?: string,
  hostTabKeys: {
    key:string,
    text:string,
  }[],
}

const HostPick: FC<Props> = memo(({
  onChange,
  defaultActiveKey = 'distribute_test',
  hostTabKeys,
}) => {
  const [activeKey, setActiveKey] = useState(defaultActiveKey);

  useEffect(() => {
    setActiveKey(defaultActiveKey);
  }, [defaultActiveKey]);

  const handleClick = (value: string) => {
    if (value !== activeKey) {
      setActiveKey(value);
      if (onChange) {
        onChange(value);
      }
    }
  };

  const getContent = () => hostTabKeys.map(({ key, text }) => (
    <>
      <div
        key={key}
        className={`c7ncd-tab-pick-item ${key === activeKey ? 'c7ncd-tab-pick-item-active' : ''}`}
        onClick={() => handleClick(key)}
      >
        <span>{text}</span>
      </div>
      <span className="c7ncd-tab-pick-item-line" />
    </>
  ));

  return (
    <div className="c7ncd-tab-pick">
      {getContent()}
    </div>
  );
});

export default HostPick;
