import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, DataSet, Menu, Dropdown, Icon, Modal,
} from 'choerodon-ui/pro';
import CustomCirculationDataSet from './stores/CustomCirculationDataSet';
import Condition from './components/condition';
import Linkage from './components/linkage';
import NotifySetting from './components/notify-setting';
import UpdateField from './components/update-field';
import styles from './index.less';
import { TabComponentProps } from '../index';

const { Column } = Table;

const CustomCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const customCirculationDataSet = useMemo(() => new DataSet(CustomCirculationDataSet), []);

  // @ts-ignore
  const getModalSetting = (key, record) => {
    const settings = {
      condition: {
        width: 380,
        title: '流转条件',
        // @ts-ignore
        children: <Condition record={record} />,
      },
      linkage: {
        width: 380,
        title: '状态联动',
        // @ts-ignore
        children: <Linkage record={record} />,
      },
      updateField: {
        width: 740,
        title: '变更属性',
        // @ts-ignore
        children: <UpdateField record={record} />,
      },
      notifySetting: {
        width: 380,
        title: '通知设置',
        // @ts-ignore
        children: <NotifySetting record={record} />,
      },
    };
    // @ts-ignore
    return settings[key];
  };

  // @ts-ignore
  const handleMenuClick = (record, e) => {
    const { title, width, children } = getModalSetting(e.key, record);
    Modal.open({
      className: `${styles[`customCirculation_${e.key}Modal`]}`,
      drawer: true,
      style: {
        width: width || 380,
      },
      key: e.key,
      title,
      children,
    });
  };

  const renderAction = ({
  // @ts-ignore
    value, text, name, record, dataSet,
  }) => {
    const menu = (
      // eslint-disable-next-line react/jsx-no-bind
      <Menu onClick={handleMenuClick.bind(this, record)}>
        <Menu.Item key="condition">流转条件</Menu.Item>
        <Menu.Item key="linkage">状态联动</Menu.Item>
        <Menu.Item key="updateField">更新属性</Menu.Item>
        <Menu.Item key="notifySetting">通知设置</Menu.Item>
      </Menu>
    );
    return (
      <Dropdown
        overlay={menu}
      >
        <Icon type="settings-o" />
      </Dropdown>
    );
  };
  return (
    <div className={`${styles.customCirculation} 111111`}>
      {tab}
      <Table dataSet={customCirculationDataSet}>
        <Column name="state" />
        <Column name="fieldsInfo" />
        <Column name="action" renderer={renderAction} />
      </Table>
    </div>
  );
};

export default observer(CustomCirculation);
