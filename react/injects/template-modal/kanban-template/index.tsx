import React, { useState, useEffect } from 'react';
import { Tabs } from 'choerodon-ui/pro';
import { kanbanTemplateApi } from '@/api';
import Detail from './detail';

const { TabPane } = Tabs;
const KanbanTemplates = () => {
  const [activeKey, setActiveKey] = useState<string | undefined>(undefined);
  const [templates, setTemplates] = useState<{
    boardId: string
    name: string
  }[]>([]);
  useEffect(() => {
    (async () => {
      const { content: res } = await kanbanTemplateApi.list(0, 0);
      setTemplates(res);
      setActiveKey(res[0]?.boardId);
    })();
  }, []);
  return (
    <div style={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
      <Tabs activeKey={activeKey} onChange={setActiveKey}>
        {templates.map((template) => (
          <TabPane key={template.boardId} tab={template.name} />
        ))}
      </Tabs>
      <div style={{ flex: 1, overflow: 'hidden' }}>
        {activeKey && <Detail templateId={activeKey} />}
      </div>
    </div>
  );
};
export default KanbanTemplates;
