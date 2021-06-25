import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';

const SettingsIndex = React.lazy(() => import('./SettingsIndex'));

class ProjectIndex extends React.Component {
  render() {
    const { match } = this.props;

    return (
      <div>
        <Switch>
          <Route path={`${match.url}`} component={SettingsIndex} />
          <Route path="*" component={nomatch} />
        </Switch>
      </div>
    );
  }
}

export default ProjectIndex;
