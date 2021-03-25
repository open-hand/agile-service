import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { ModalContainer } from 'choerodon-ui/pro';
import { asyncLocaleProvider, stores, nomatch } from '@choerodon/boot';
import 'moment/locale/zh-cn';
import 'moment/locale/en-nz';
import moment from 'moment';
import { PermissionRoute } from '@choerodon/master';
import AgileProvider from '@/components/AgileProvider';
import { setHistory } from '@/utils/to';
import './style/index.less';

const ScrumBoard = React.lazy(() => import('./routes/ScrumBoard'));
const ReportHost = React.lazy(() => import('./routes/ReportHost'));
const StoryMap = React.lazy(() => import('./routes/StoryMap'));
const WorkList = React.lazy(() => import('./routes/WorkList'));
const IssueType = React.lazy(() => import('./routes/issueType'));
const Priority = React.lazy(() => import('./routes/priority'));
const State = React.lazy(() => import('./routes/state'));
const PageConfig = React.lazy(() => import('./routes/page-config'));
const StateMachine = React.lazy(() => import('./routes/StateMachine'));
const TeamPerformance = React.lazy(() => import('./routes/TeamPerformance'));
const VersionList = React.lazy(() => (import('./routes/version-list')));
// 敏捷设置
const Settings = React.lazy(() => import('./routes/settings'));
const ProjectReport = React.lazy(() => import('./routes/project-report'));
const GanttPage = React.lazy(() => import('./routes/gantt'));
const UiPreview = React.lazy(() => import('./routes/ui-preview'));
const KanbanTemplateDetail = React.lazy(() => import('./routes/kanban-template/detail'));
const { AppState } = stores;
export function getRoutes(match) {
  return [
    <Route
      path={`${match.url}/work-list`}
      component={WorkList}
    />,
    <Route path={`${match.url}/storyMap`} component={StoryMap} />,
    <Route path={`${match.url}/team-performance`} component={TeamPerformance} />,
    <Route path={`${match.url}/scrumboard`} component={ScrumBoard} />,
    <Route path={`${match.url}/gantt`} component={GanttPage} />,
    <Route path={`${match.url}/project-version`} component={VersionList} />,
    <Route path={`${match.url}/reporthost`} component={ReportHost} />,
    <PermissionRoute
      service={(type) => (
        type === 'project' ? [
          'choerodon.code.project.setting.page.ps.field',
          'choerodon.code.project.setting.page.ps.scheme',
        ] : [
          'choerodon.code.organization.setting.issue.page.ps.scheme',
          'choerodon.code.organization.setting.issue.page.ps.filed',
        ]
      )}
      path={`${match.url}/page`}
      component={PageConfig}
    />,
    <Route path={`${match.url}/issue-type`} component={IssueType} />,
    <Route path={`${match.url}/settings`} component={Settings} />,
    <PermissionRoute
      service={[]}
      path={`${match.url}/kanban-template/detail/:templateId`}
      component={KanbanTemplateDetail}
    />,
    <Route path={`${match.url}/states`} component={State} />,
    <Route path={`${match.url}/priorities`} component={Priority} />,
    <PermissionRoute
      service={['choerodon.code.project.setting.state.ps.default',
        'choerodon.code.project.setting.state.ps.master']}
      path={`${match.url}/state-machine`}
      component={StateMachine}
    />,
    <Route path={`${match.url}/project-report`} component={ProjectReport} />,
    <Route path={`${match.url}/ui-preview/:uuid`} component={UiPreview} />,
    <Route path={`${match.url}/outside/ui-preview/:uuid`} component={UiPreview} />,
  ];
}
class Agile extends React.Component {
  constructor(props) {
    super(props);
    setHistory(props.history);
  }

  // componentDidCatch(error, info) {
  //   Choerodon.prompt(error.message);
  // }

  render() {
    const { match } = this.props;
    const language = AppState.currentLanguage;
    if (language === 'zh_CN') {
      moment.locale('zh-cn');
    }
    const IntlProviderAsync = asyncLocaleProvider(language, () => import(`./locale/${language}`));
    return (
      <div id="agile">
        <AgileProvider projectId={AppState.currentMenuType.id}>
          <IntlProviderAsync>
            <Switch>
              {/* 协作 */}
              {getRoutes(match)}
              <Route path="*" component={nomatch} />
            </Switch>
            <ModalContainer />
          </IntlProviderAsync>
        </AgileProvider>
      </div>
    );
  }
}
export * from './exports';
export * from '@/hooks';
export * from '@/components';

export default Agile;
