import React, { useCallback, useState } from 'react';
import { Button, Modal, Tabs } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import StatusMachineTemplate from '../../routes/statusMachine-template';
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
      <Tabs activeKey={activeKey} onChange={handleActiveKeyChange} className={styles.template_tab}>
        {
          ['statusMachineTemplate', 'boardTemplate'].map((item) => (
            <TabPane key={item} tab={templateTabMap.get(item)?.name} />
          ))
        }
      </Tabs>
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
    children: <ObserverTemplateContent {...props} />,
    okText: '关闭',
    footer: (okBtn: Button) => okBtn,
  });
};

export default openTemplate;
