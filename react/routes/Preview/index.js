import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import {
  asyncRouter, nomatch, asyncLocaleProvider, stores, 
} from '@choerodon/boot';

const Preview = asyncRouter(() => (import('./PreviewHome/index')));

const PreviewIndex = ({ match }) => {
  const { AppState } = stores;
  const langauge = AppState.currentLanguage;
  const IntlProviderAsync = asyncLocaleProvider(langauge, () => import(`../../locale/${langauge}`));
  return (
    <div style={{ height: '100%' }}>
      <IntlProviderAsync>
        <Switch>
          <Route exact path={`${match.url}`} component={Preview} />
          <Route path="*" component={nomatch} />
        </Switch>
      </IntlProviderAsync>
    </div>
  
  );
};

export default PreviewIndex;
