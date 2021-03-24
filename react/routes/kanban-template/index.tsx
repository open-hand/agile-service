import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const KanbanTemplateList = asyncRouter(() => import('./list'));
const KanbanTemplateCreate = asyncRouter(() => import('./create'));

const KanbanTemplate = () => {
  const url = '/agile/states/kanban';
  return (
    <Switch>
      <PermissionRoute
        exact
        service={[]}
        path={url}
        component={KanbanTemplateList}
      />
      <PermissionRoute
        exact
        service={[]}
        path={`${url}/create`}
        component={KanbanTemplateCreate}
      />
      <Route path="*" component={nomatch} />
    </Switch>
  );
};

export default KanbanTemplate;
