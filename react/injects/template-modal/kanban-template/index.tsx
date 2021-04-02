import React, { useState, useEffect, useMemo } from 'react';
import Tabs from '@/components/tabs';
import { kanbanTemplateApi } from '@/api';
import Empty from '@/components/Empty';
import Detail from './detail';
import empty from './empty.svg';

const KanbanTemplates = () => {
  const [activeKey, setActiveKey] = useState<string | undefined>(undefined);
  const [templates, setTemplates] = useState<{
    boardId: string
    name: string
  }[]>([]);
  const [loading, setLoading] = useState<boolean>(false);

  useEffect(() => {
    (async () => {
      setLoading(true);
      const { content: res } = await kanbanTemplateApi.list(0, 0);
      setLoading(false);
      setTemplates(res);
      setActiveKey(res[0]?.boardId);
    })();
  }, []);
  const tabs = useMemo(() => templates.map((template) => ({ key: template.boardId, title: template.name })), [templates]);
  return (
    <div style={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
      {
        !loading && (
        <>
          {
            templates.length > 0 ? (
              <>
                <Tabs activeKey={activeKey} onChange={setActiveKey} tabs={tabs} color="#5365EA" />
                <div style={{ flex: 1, overflow: 'hidden' }}>
                  {activeKey && <Detail templateId={activeKey} />}
                </div>
              </>
            ) : (
              <Empty
                pic={empty}
                title="未设置看板模板"
                description="未设置看板模板，将为您预设为系统看板"
                imgStyle={{ width: 175 }}
              />
            )
          }
        </>
        )
      }
    </div>
  );
};
export default KanbanTemplates;
