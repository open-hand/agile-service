import React, { useState, useEffect, useMemo } from 'react';
import Tabs from '@/components/tabs';
import { kanbanTemplateApi } from '@/api';
import { EmptyPage } from '@choerodon/components';
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
    <div style={{
      height: 'calc(100% - 41px)',
      display: 'flex',
      flexDirection: 'column',
      marginTop: '0.2rem',
    }}
    >
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
              <EmptyPage
                image={empty}
                description="未设置看板模板，将为您预设为系统看板"
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
