import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { Page, Header, Content } from '@choerodon/boot';
import {
  Table, DataSet, Menu, Dropdown, Icon, Modal,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Breadcrumb } from 'choerodon-ui';
import { useIssueTypes } from '@/hooks';
import { find } from 'lodash';
import { IIssueType } from '@/common/types';
import { useIsProgramContext } from '@/hooks/useIsProgrom';
import Condition from './components/condition';
import Linkage from './components/linkage';
import NotifySetting from './components/notify-setting';
import UpdateField from './components/update-field';
import IssueTypeTab from '../components/issue-type-tab';
import { useStateMachineContext } from '../context';
import styles from './index.less';
import { TabComponentProps } from '../index';

const { Column } = Table;

interface ISetting {
  width: number,
  title: string,
  children: JSX.Element,
}
interface ModalSettings {
  condition: ISetting,
  linkage: ISetting,
  updateField: ISetting,
  notifySetting: ISetting,
}
const CustomCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const { isProgram } = useIsProgramContext();
  const [issueTypes] = useIssueTypes();
  const { selectedType, setSelectedType } = useStateMachineContext();
  const customCirculationDataSet = useMemo(() => new DataSet({
    selection: false,
    fields: [
      {
        name: 'state',
        label: '状态',
        type: 'string' as FieldType,
      },
      {
        name: 'fieldsInfo',
        label: '状态流转附加字段信息',
        type: 'array' as FieldType,
      },
      {
        name: 'action',
        label: '自定义操作',
        type: 'string' as FieldType,
      },
    ],
    queryFields: [
      {
        name: 'state',
        label: '状态',
        type: 'string' as FieldType,
      },
    ],
    data: [
      {
        state: '待处理',
        fieldsInfo: [],
      },
      {
        state: '处理中',
        fieldsInfo: [],
      },
    ],
  }), []);

  // @ts-ignore
  const getModalSetting = (key: 'condition' | 'linkage' | 'updateField' | 'notifySetting', record) => {
    const settings: ModalSettings = {
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
        children: <Linkage record={record} selectedType={selectedType} />,
      },
      updateField: {
        width: 740,
        title: '变更属性',
        // @ts-ignore
        children: <UpdateField record={record} isProgram={isProgram} />,
      },
      notifySetting: {
        width: 380,
        title: '通知设置',
        // @ts-ignore
        children: <NotifySetting record={record} />,
      },
    };
    return settings[key];
  };

  const handleMenuClick = (record: any, e: { key: 'condition' | 'linkage' | 'updateField' | 'notifySetting' }) => {
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
    const selectedTypeCode = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType)?.typeCode;
    const menu = (
      // eslint-disable-next-line react/jsx-no-bind
      <Menu onClick={handleMenuClick.bind(this, record)}>
        <Menu.Item key="condition">流转条件</Menu.Item>
        {
          (selectedTypeCode === 'sub_task' || selectedTypeCode === 'bug') && (
            <Menu.Item key="linkage">状态联动</Menu.Item>
          )
        }
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
    <Page>
      <Content>
        <Breadcrumb />
        <IssueTypeTab selectedType={selectedType} setSelectedType={setSelectedType} />
        {tab}
        <div className={`${styles.customCirculation}`}>
          <Table dataSet={customCirculationDataSet}>
            <Column name="state" />
            <Column name="fieldsInfo" />
            <Column name="action" renderer={renderAction} />
          </Table>
        </div>
      </Content>
    </Page>
  );
};

export default observer(CustomCirculation);
