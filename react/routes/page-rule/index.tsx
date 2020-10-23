import React, { useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { Button, DataSet, Modal } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { pageRuleApiConfig } from '@/api';
import { getApplyType } from '@/utils/common';
import IsProgramContext from '@/hooks/useIsProgrom';
import RuleTable from './components/rule-table';
import RuleModal from './components/rule-modal';
import styles from './index.less';

const PageRule = () => {
  const ruleTableDataSet = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    transport: {
      read: ({ params }) => pageRuleApiConfig.load(params),
    },
    fields: [
      {
        label: '名称',
        name: 'name',
        type: 'string' as FieldType,
      },
      {
        name: 'action',
      },
      {
        label: '通知规则',
        name: 'expressQuery',
        type: 'string' as FieldType,
      },
      {
        label: '问题类型',
        name: 'issueTypes',
        type: 'array' as FieldType,
      },
      {
        label: '变更值',
        name: 'processerList',
      },
      {
        label: '通知对象',
        name: 'receiverList',
        type: 'array' as FieldType,
      },
      {
        label: '状态',
        name: 'enabled',
        type: 'boolean' as FieldType,
      },
      {
        label: '来源',
        name: 'source',
        type: 'string' as FieldType,
      },
    ],
    queryFields: [
      {
        name: 'name',
        label: '名称',
      },
      {
        name: 'enabled',
        label: '状态',
        type: 'string' as FieldType,
        textField: 'label',
        valueField: 'value',
        options: new DataSet({
          data: [
            { label: '启用', value: 'true' },
            { label: '停用', value: 'false' },
          ],
        }),
      },
      {
        name: 'source',
        label: '来源',
        textField: 'label',
        valueField: 'value',
        type: 'string' as FieldType,
        options: new DataSet({
          data: [
            { label: '自定义', value: 'custom' },
            { label: '预定义', value: 'predefined' },
          ],
        }),
      },
    ],
  }), []);

  const handleAddRule = () => {
    Modal.open({
      className: styles.rule_modal,
      drawer: true,
      style: {
        width: 740,
      },
      key: Modal.key(),
      title: '添加规则',
      children: <RuleModal ruleTableDataSet={ruleTableDataSet} isProgram={getApplyType() === 'program'} />,
    });
  };

  return (
    <IsProgramContext.Provider value={{ isProgram: getApplyType() === 'program' }}>
      <Page service={[]} className={styles.pageRule}>
        <Header>
          <Button icon="playlist_add" onClick={handleAddRule}>添加规则</Button>
        </Header>
        <Breadcrumb />
        <Content>
          <RuleTable tableDataSet={ruleTableDataSet} />
        </Content>
      </Page>
    </IsProgramContext.Provider>

  );
};

export default observer(PageRule);
