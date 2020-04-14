import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { ModalContainer } from 'choerodon-ui/pro';
import {
  asyncRouter, asyncLocaleProvider, stores, nomatch, 
} from '@choerodon/boot';
import 'moment/locale/zh-cn';
import 'moment/locale/en-nz';
import moment from 'moment';
import AgileProvider from '@/components/AgileProvider';
import IsInProgramStore from './stores/common/program/IsInProgramStore';
import RunWhenProjectChange from './common/RunWhenProjectChange';
import './style/index.less';

const ScrumBoard = asyncRouter(() => import('./routes/ScrumBoard'));
const ReportHost = asyncRouter(() => import('./routes/ReportHost'));
const StoryMap = asyncRouter(() => import('./routes/StoryMap'));
const IterationBoard = asyncRouter(() => import('./routes/IterationBoard'));
const WorkList = asyncRouter(() => import('./routes/WorkList'));
const IssueType = asyncRouter(() => import('./routes/issueType'));
const Priority = asyncRouter(() => import('./routes/priority'));
const State = asyncRouter(() => import('./routes/state'));
const PageConfig = asyncRouter(() => import('./routes/page-config'));
// 敏捷设置
const Settings = asyncRouter(() => import('./routes/settings'));

const { AppState } = stores;

class Index extends React.Component {
  // componentDidCatch(error, info) {
  //   Choerodon.prompt(error.message);
  // }

  componentDidMount() {
    if (process.env.NODE_ENV === 'development') {
      if (AppState.currentMenuType.category !== 'PROGRAM') {
        // 切换项目查是否在项目群中
        RunWhenProjectChange(IsInProgramStore.refresh);
        IsInProgramStore.refresh();
      }
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
        <AgileProvider>
          <IntlProviderAsync>
            <Switch>
              {/* 协作 */}
              <Route path={`${match.url}/work-list`} component={WorkList} />
              <Route path={`${match.url}/storyMap`} component={StoryMap} />
              {/* 开发-迭代计划 */}
              <Route path={`${match.url}/scrumboard`} component={ScrumBoard} />
              <Route path={`${match.url}/iterationBoard/:id`} component={IterationBoard} />
              {/* 运营 */}
              <Route path={`${match.url}/reporthost`} component={ReportHost} />
              {/* 设置页面 */}
              <Route path={`${match.url}/page`} component={PageConfig} />
              {/* 页面类型 */}
              <Route path={`${match.url}/issue-type`} component={IssueType} />
              <Route path={`${match.url}/settings`} component={Settings} />            
              <Route path={`${match.url}/states`} component={State} />
              <Route path={`${match.url}/priorities`} component={Priority} />
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

export default Index;
