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
import './style/index.less';

const ScrumBoard = asyncRouter(() => import('./routes/ScrumBoard'));
const IterationBoard = asyncRouter(() => import('./routes/IterationBoard'));
const WorkList = asyncRouter(() => import('./routes/WorkList'));
const StoryMap = asyncRouter(() => import('./routes/StoryMap'));
const ReportHost = asyncRouter(() => import('./routes/ReportHost'));
const PageConfig = asyncRouter(() => import('./routes/page-config'));
const IssueTypeIndex = asyncRouter(() => import('./routes/issueType'));
const settings = asyncRouter(() => import('./routes/settings'));
const StateIndex = asyncRouter(() => import('./routes/state'));
const PriorityIndex = asyncRouter(() => import('./routes/priority'));

const { AppState } = stores;
class Index extends React.Component {
  componentDidCatch(error, info) {
    // Choerodon.prompt(error.message);
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
        <IntlProviderAsync>
          <Switch>
            {/* 协作-迭代计划 */}
            <Route path={`${match.url}/scrumboard`} component={ScrumBoard} />
            {/* 协作-迭代计划-迭代工作台 */}
            <Route path={`${match.url}/iterationBoard/:id`} component={IterationBoard} />
            {/* 协作-工作列表 */}
            <Route path={`${match.url}/work-list`} component={WorkList} />
            {/* 协作-故事地图 */}
            <Route path={`${match.url}/storyMap`} component={StoryMap} />
            {/* 运营-图表 */}
            <Route path={`${match.url}/reporthost`} component={ReportHost} />
            {/* 设置-页面 */}
            <Route path={`${match.url}/page`} component={PageConfig} />
            {/* 页面类型 */}
            <Route path={`${match.url}/issue-type`} component={IssueTypeIndex} />
            {/* 问题设置 */}
            <Route path={`${match.url}/settings`} component={settings} />
            {/* 管理中心-问题-状态机 */}
            <Route path={`${match.url}/states`} component={StateIndex} />
            {/* 管理中心-问题-优先级 */}
            <Route path={`${match.url}/priorities`} component={PriorityIndex} />
            <Route path="*" component={nomatch} />
          </Switch>
        </IntlProviderAsync>
        <ModalContainer />
      </div>
    );
  }
}

export default Index;
