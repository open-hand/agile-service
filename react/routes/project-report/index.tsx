import React from 'react';
import { Route, Switch, RouteChildrenProps } from 'react-router-dom';
import { nomatch } from '@choerodon/boot';
import { addChartsMap, ChartMap, defaultCharts } from './report-page/components/add-chart';

const ReportList = React.lazy(() => import('./report-list'));
const CreateReport = React.lazy(() => import('./report-create'));
const EditReport = React.lazy(() => import('./report-edit'));
const PreviewReport = React.lazy(() => import('./report-preview'));

const ProjectReport: React.FC<RouteChildrenProps> = ({ match }) => (
  <Switch>
    <Route exact path={match?.url} component={ReportList} />
    <Route exact path={`${match?.url}/create`} component={CreateReport} />
    <Route exact path={`${match?.url}/edit/:id`} component={EditReport} />
    <Route exact path={`${match?.url}/preview/:id`} component={PreviewReport} />
    <Route path="*" component={nomatch} />
  </Switch>
);
export { addChartsMap, ChartMap, defaultCharts };
export default ProjectReport;
