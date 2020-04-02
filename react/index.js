import React, { useRef, useEffect } from 'react';
import ReactDOM from 'react-dom';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { ModalContainer } from 'choerodon-ui/pro';
import {
  asyncRouter, asyncLocaleProvider, stores, nomatch, Choerodon,
} from '@choerodon/boot';
import 'moment/locale/zh-cn';
import 'moment/locale/en-nz';
import moment from 'moment';
import IsInProgramStore from './stores/common/program/IsInProgramStore';
import RunWhenProjectChange from './common/RunWhenProjectChange';
import './style/index.less';

const ScrumBoard = asyncRouter(() => import('./routes/ScrumBoard'));
const ReportHost = asyncRouter(() => import('./routes/ReportHost'));
const StoryMap = asyncRouter(() => import('./routes/StoryMap'));
const IterationBoard = asyncRouter(() => import('./routes/IterationBoard'));
const WorkList = asyncRouter(() => import('./routes/WorkList'));
const IssueTypeIndex = asyncRouter(() => import('./routes/issueType'));
const PriorityIndex = asyncRouter(() => import('./routes/priority'));
const StateIndex = asyncRouter(() => import('./routes/state'));
const PageConfig = asyncRouter(() => import('./routes/page-config'));
// 敏捷设置
const settings = asyncRouter(() => import('./routes/settings'));

const { AppState } = stores;
function Test() {
  const ref = useRef();
  useEffect(() => {
    ReactDOM.createPortal(<div>scroll</div>, ref.current);
  }, []);
  return (
    <div ref={ref}>
      <div className="body" />
    </div>
  );
}
class Index extends React.Component {
  // componentDidCatch(error, info) {
  //   Choerodon.prompt(error.message);
  // }

  componentDidMount() {
    if (AppState.currentMenuType.category !== 'PROGRAM') {
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
            <Route path={`${match.url}/issue-type`} component={IssueTypeIndex} />
            <Route path={`${match.url}/settings`} component={settings} />            
            <Route path={`${match.url}/states`} component={StateIndex} />
            <Route path={`${match.url}/priorities`} component={PriorityIndex} />
            <Route path={`${match.url}/test`} component={Test} />
            <Route path="*" component={nomatch} />
          </Switch>
        </IntlProviderAsync>
        <ModalContainer />
      </div>
    );
  }
}

export default Index;
