import React from 'react';
import {
  Route,
  Switch,
  useRouteMatch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const KanbanTemplateList = asyncRouter(() => import('./list'));
const KanbanTemplateCreate = asyncRouter(() => import('./create'));

const KanbanTemplate = () => {
  const match = useRouteMatch();
  return (
    <Switch>
      <PermissionRoute
        exact
        service={[]}
        path={match.url}
        component={KanbanTemplateList}
      />
      <PermissionRoute
        exact
        service={[]}
        path={`${match.url}/create`}
        component={KanbanTemplateCreate}
      />
      <Route path="*" component={nomatch} />
    </Switch>
  );
};

export default KanbanTemplate;
