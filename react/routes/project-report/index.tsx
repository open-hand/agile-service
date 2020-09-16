import React from 'react';
import { Route, Switch, RouteChildrenProps } from 'react-router-dom';
import { nomatch } from '@choerodon/boot';

const ReportList = React.lazy(() => import('./report-list'));
const CreateReport = React.lazy(() => import('./create-report'));

const ProjectReport: React.FC<RouteChildrenProps> = ({ match }) => (
  <Switch>
    <Route exact path={match?.url} component={ReportList} />
    <Route exact path={`${match?.url}/create`} component={CreateReport} />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default ProjectReport;
