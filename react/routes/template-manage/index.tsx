import React from 'react';
import { withRouter } from 'react-router-dom';
import {
  asyncRouter, PageWrap, PageTab,
} from '@choerodon/boot';

const KanbanTemplate = withRouter(asyncRouter(() => import('../kanban-template')));

const TemplateManage = () => (
  <PageWrap
    noHeader={[]}
  >
    <PageTab
      title="看板模板"
      route="/agile/template-manage/kanban"
      tabKey="choerodon.code.project.cooperation.work-list.ps.backlog"
      component={KanbanTemplate}
    />
  </PageWrap>
);
export default TemplateManage;
