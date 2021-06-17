import React from 'react';
import { Route, Switch } from 'react-router-dom';
import { asyncRouter, nomatch, Charts } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const ReportHostHome = asyncRouter(() => (import('./Home')));
const BurndownChart = asyncRouter(() => (import('./BurndownChart')));
const sprintReport = asyncRouter(() => (import('./SprintReport')));
const Accumulation = asyncRouter(() => (import('./Accumulation')));
const VelocityReport = asyncRouter(() => (import('./VelocityChart')));
const EpicReport = asyncRouter(() => (import('./EpicReport')));
const PieChartReport = asyncRouter(() => (import('./pieChart')));
const VersionReport = asyncRouter(() => (import('./VersionReport')));
const EpicBurndown = asyncRouter(() => (import('./EpicBurndown')));
const VersionBurndown = asyncRouter(() => (import('./VersionBurndown')));

const ReportHostIndex = ({ match }) => (
  <Switch>
    <Route exact path={match.url} component={() => <Charts reportType="agile" />} />
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
    <Route path="*" component={nomatch} />
  </Switch>
);
export default ReportHostIndex;
