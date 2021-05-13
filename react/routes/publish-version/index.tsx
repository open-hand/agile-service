import React from 'react';
import { Route, Switch, RouteChildrenProps } from 'react-router-dom';
import { nomatch } from '@choerodon/boot';

const PublishVersion = React.lazy(() => import('./PublishVersion'));
const PublishVersionPreview = React.lazy(() => import('./PublishVersionPreview'));

const ProjectReport: React.FC<RouteChildrenProps> = ({ match }) => {
  console.log('match', match?.url, match);
  return (
    <Switch>
      <Route exact path={match?.url} component={PublishVersion} />
      <Route exact path={`${match?.url}/preview/:id`} component={PublishVersionPreview} />
      <Route path="*" component={nomatch} />
    </Switch>
  );
};

export default PublishVersion;
