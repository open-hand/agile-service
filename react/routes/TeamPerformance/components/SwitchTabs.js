/* eslint-disable */
import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import './SwitchTabs.less';

const prefixCls = 'c7n-team-performance-wrap';
const SwitchTabs = observer((props) => {
  const {
    onChange, dataSet, field = 'tab', ...other
  } = props;
  const tabsData = dataSet.getField(field).get('options').toData();

  const handleChangeActiveKey = (key) => {
    dataSet.current.set(field, key);
    onChange();
  };

  return (
    <ul className={`${prefixCls}-switch`} {...other}>
      {
        tabsData.map((tabItem, index) => (
          <>
            <li
              className={classnames(`${prefixCls}-switch-li`, {'c7n-team-performance-wrap-switch-li-active':  dataSet.current.get(field) === tabItem.value })}
              onClick={() => handleChangeActiveKey(tabItem.value)}
              key={tabItem.value}
            >
              {tabItem.meaning}
            </li>
            { (index !== tabsData.length - 1) && <span className="line" /> }
          </>
        ))
      }
    </ul>
  );
});

export default SwitchTabs;
