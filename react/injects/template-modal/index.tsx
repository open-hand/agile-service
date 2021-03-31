import React, { useCallback, useState } from 'react';
import { Button, Modal, Tabs } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import StatusMachineTemplate from '../../routes/statusMachine-template';
// const StatusMachineTemplate = React.lazy(() => import('../../routes/statusMachine-template'));

interface Props {
  templateTabsKey: string[]
}

const { TabPane } = Tabs;

const templateTabMap = new Map([
  ['statusMachineTemplate', {
    name: '状态机模板',
  }],
  ['boardTemplate', {
    name: '看板模板',
  }],
]);

const Test = () => (<div>test</div>);
const TemplateContent: React.FC<Props> = ({ templateTabsKey }) => {
  const [activeKey, setActiveKey] = useState<'statusMachineTemplate' | 'boardTemplate'>('statusMachineTemplate');
  const handleActiveKeyChange = useCallback((key) => {
    setActiveKey(key);
  }, []);
  return (
    <>
      <Tabs activeKey={activeKey} onChange={handleActiveKeyChange}>
        {
          templateTabsKey.map((item) => (
            <TabPane key={item} tab={templateTabMap.get(item)?.name} />
          ))
        }
      </Tabs>
      <div>
        {
          activeKey === 'statusMachineTemplate' ? (
            // <Test />
            <StatusMachineTemplate readOnly />
            // <div>状态机看板</div>
          ) : '看板模板'
        }
      </div>
    </>
  );
};

const ObserverTemplateContent = observer(TemplateContent);

const openTemplate = (props: Props) => {
  Modal.open({
    drawer: true,
    key: Modal.key(),
    style: {
      width: '1090px',
    },
    title: '模板',
    children: <ObserverTemplateContent {...props} />,
    okText: '关闭',
    footer: (okBtn: Button) => okBtn,
  });
};

export default openTemplate;
