import React, { useCallback, useState } from 'react';
import { Button, Modal, Tabs } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import HostPick from '@/components/host-pick';
import StatusMachineTemplate from '../../routes/statusMachine-template';
import IntlProviderAsync from '../../components/intl-provider-async';
import styles from './index.less';
import KanbanTemplate from './kanban-template';

const { TabPane } = Tabs;

const templateTabMap = new Map([
  ['statusMachineTemplate', {
    name: '状态机模板',
  }],
  ['boardTemplate', {
    name: '看板模板',
  }],
]);

const TemplateContent = () => {
  const [activeKey, setActiveKey] = useState<'statusMachineTemplate' | 'boardTemplate'>('statusMachineTemplate');
  const handleActiveKeyChange = useCallback((key) => {
    setActiveKey(key);
  }, []);
  return (
    <>
      <HostPick
        defaultActiveKey="statusMachineTemplate"
        onChange={handleActiveKeyChange}
        hostTabKeys={[{
          key: 'statusMachineTemplate',
          text: templateTabMap.get('statusMachineTemplate')?.name as string,
        }, {
          key: 'boardTemplate',
          text: templateTabMap.get('boardTemplate')?.name as string,
        }]}
      />
      {
        activeKey === 'statusMachineTemplate' && (
          <StatusMachineTemplate readOnly visibleIssueTypeCategory="initial" noContainer />
        )
      }
      {
        activeKey === 'boardTemplate' && (
          <KanbanTemplate />
        )
      }
    </>
  );
};

const ObserverTemplateContent = observer(TemplateContent);

const openTemplate = (props: any) => {
  Modal.open({
    drawer: true,
    key: Modal.key(),
    style: {
      width: '1090px',
    },
    title: '查看模板',
    className: styles['c7nagile-template-modal'],
    children: (
      <IntlProviderAsync>
        <ObserverTemplateContent {...props} />
      </IntlProviderAsync>),
    okText: '关闭',
    footer: (okBtn: Button) => okBtn,
  });
};

export default openTemplate;
