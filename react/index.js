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
import AgileProvider from '@/components/AgileProvider';
import RunWhenProjectChange from '@/common/RunWhenProjectChange';
import { setHistory } from '@/utils/to';
import IsInProgramStore from './stores/common/program/IsInProgramStore';
import './style/index.less';

const ScrumBoard = React.lazy(() => import('./routes/ScrumBoard'));
const ReportHost = React.lazy(() => import('./routes/ReportHost'));
const StoryMap = React.lazy(() => import('./routes/StoryMap'));
const IterationBoard = React.lazy(() => import('./routes/IterationBoard'));
const WorkList = React.lazy(() => import('./routes/WorkList'));
const IssueType = React.lazy(() => import('./routes/issueType'));
const Priority = React.lazy(() => import('./routes/priority'));
const State = React.lazy(() => import('./routes/state'));
const PageConfig = React.lazy(() => import('./routes/page-config'));
const StateMachine = React.lazy(() => import('./routes/StateMachine'));
const TeamPerformance = React.lazy(() => import('./routes/TeamPerformance'));
const Release = React.lazy(() => (import('./routes/Release')));
// 敏捷设置
const Settings = React.lazy(() => import('./routes/settings'));
const ProjectReport = React.lazy(() => import('./routes/project-report'));
const GanttPage = React.lazy(() => import('./routes/gantt'));
const UiPreview = React.lazy(() => import('./routes/ui-preview'));

const { AppState } = stores;

class Agile extends React.Component {
  constructor(props) {
    super(props);
    setHistory(props.history);
  }

  // componentDidCatch(error, info) {
  //   Choerodon.prompt(error.message);
  // }

  componentDidMount() {
    if (process.env.NODE_ENV === 'development') {
      // 切换项目查是否在项目群中
      RunWhenProjectChange(IsInProgramStore.refresh);
      IsInProgramStore.refresh();
    }
  }

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
              <Route path={`${match.url}/work-list`} component={WorkList} />
              <Route path={`${match.url}/storyMap`} component={StoryMap} />
              <Route path={`${match.url}/team-performance`} component={TeamPerformance} />
              {/* 开发-迭代计划 */}
              <Route path={`${match.url}/scrumboard`} component={ScrumBoard} />
              <Route path={`${match.url}/iterationBoard/:id`} component={IterationBoard} />
              <Route path={`${match.url}/gantt`} component={GanttPage} />
              <Route path={`${match.url}/project-version`} component={Release} />
              {/* 运营 */}
              <Route path={`${match.url}/reporthost`} component={ReportHost} />
              {/* 设置页面 */}
              <Route path={`${match.url}/page`} component={PageConfig} />
              {/* 页面类型 */}
              <Route path={`${match.url}/issue-type`} component={IssueType} />
              <Route path={`${match.url}/settings`} component={Settings} />
              <Route path={`${match.url}/states`} component={State} />
              <Route path={`${match.url}/priorities`} component={Priority} />
              <Route path={`${match.url}/state-machine`} component={StateMachine} />
              <Route path={`${match.url}/project-report`} component={ProjectReport} />
              <Route path={`${match.url}/ui-preview/:uuid`} component={UiPreview} />
              <Route path="*" component={nomatch} />
            </Switch>
          </IntlProviderAsync>
          <ModalContainer />
        </AgileProvider>
      </div>
    );
  }
}
export * from './exports';
export * from '@/hooks';
export * from '@/components';

export default Agile;
