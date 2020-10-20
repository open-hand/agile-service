import React, { useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { Button, DataSet, Modal } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { pageRuleApiConfig } from '@/api';
import RuleTable from './components/rule-table';
import styles from './index.less';
import RuleModal from './components/rule-modal';

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
        label: '变更值',
        name: 'newAssignee',
      },
      {
        label: '通知对象',
        name: 'receiverList',
        type: 'array' as FieldType,
      },
      // {
      //   label: '抄送人',
      //   name: 'ccList',
      //   type: 'string' as FieldType,
      // },
      {
        label: '状态',
        name: 'enabled',
        type: 'boolean' as FieldType,
      },
      {
        label: '来源',
        name: 'source',
        type: 'boolean' as FieldType,
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
      children: <RuleModal ruleTableDataSet={ruleTableDataSet} />,
    });
  };

  return (
    <Page service={[]}>
      <Header>
        <Button icon="playlist_add" onClick={handleAddRule}>添加规则</Button>
      </Header>
      <Breadcrumb />
      <Content>
        <RuleTable tableDataSet={ruleTableDataSet} />
      </Content>
    </Page>
  );
};

export default observer(PageRule);
