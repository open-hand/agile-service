import React from 'react';
import { Route, Switch } from 'react-router-dom';
import { nomatch, Charts } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';
import { useTabActiveKey } from '@choerodon/components';
import useIsProgram from '@/hooks/useIsProgram';
import { get } from '@choerodon/inject';

const ReportRoutes = get('agile:ProgramReportRoutes');
const ReportAdd = React.lazy(() => import('./custom-report/AddReport'));
const BurndownChart = React.lazy(() => (import('./BurndownChart')));
const sprintReport = React.lazy(() => (import('./SprintReport')));
const Accumulation = React.lazy(() => (import('./Accumulation')));
const VelocityReport = React.lazy(() => (import('./VelocityChart')));
const EpicReport = React.lazy(() => (import('./EpicReport')));
const PieChartReport = React.lazy(() => (import('./pieChart')));
const VersionReport = React.lazy(() => (import('./VersionReport')));
const EpicBurndown = React.lazy(() => (import('./EpicBurndown')));
const VersionBurndown = React.lazy(() => (import('./VersionBurndown')));
const Main = () => {
  const { isProgram } = useIsProgram();
  useTabActiveKey(isProgram ? 'program' : 'agile');
  return <Charts reportType="agile" />;
};
const ReportHostIndex = ({ match }) => (
  <Switch>
    <Route exact path={match.url} component={Main} />
    <PermissionRoute
      service={[]}
      path={`${match.url}/add`}
      component={ReportAdd}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.burndown']}
      path={`${match.url}/burndownchart`}
      component={BurndownChart}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.sprintreport']}
      path={`${match.url}/sprintreport`}
      component={sprintReport}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.cumulative_flow_diagram']}
      path={`${match.url}/accumulation`}
      component={Accumulation}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.velocity_chart']}
      path={`${match.url}/velocityChart`}
      component={VelocityReport}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.epicreport']}
      path={`${match.url}/EpicReport`}
      component={EpicReport}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.piechart']}
      path={`${match.url}/pieReport`}
      component={PieChartReport}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.versionreport']}
      path={`${match.url}/versionReport`}
      component={VersionReport}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.epicburndown']}
      path={`${match.url}/epicBurndown`}
      component={EpicBurndown}
    />
    <PermissionRoute
      service={['choerodon.code.project.operation.chart.ps.choerodon.code.project.operation.chart.ps.versionburndown']}
      path={`${match.url}/versionBurndown`}
      component={VersionBurndown}
    />
    {ReportRoutes && ReportRoutes({ match })}
    <Route path="*" component={nomatch} />
  </Switch>
);
export default ReportHostIndex;
