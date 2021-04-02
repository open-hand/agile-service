import React, { useState, useEffect, useMemo } from 'react';
import Tabs from '@/components/tabs';
import { kanbanTemplateApi } from '@/api';
import Detail from './detail';

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
  const tabs = useMemo(() => templates.map((template) => ({ key: template.boardId, title: template.name })), [templates]);
  return (
    <div style={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
      <Tabs activeKey={activeKey} onChange={setActiveKey} tabs={tabs} color="#5365EA" />
      <div style={{ flex: 1, overflow: 'hidden' }}>
        {activeKey && <Detail templateId={activeKey} />}
      </div>
    </div>
  );
};
export default KanbanTemplates;
