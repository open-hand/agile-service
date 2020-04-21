import React, { Component } from 'react';
import { observer } from 'mobx-react-lite';
import PropTypes from 'prop-types';

import './SprintIcon.less';

const SprintIcon = observer(({ sprintType }) => (sprintType === 'ip' ? (
  <div className="c7n-backlog-sprintIcon">
    <span>IP</span>
  </div>
) : null));

export default SprintIcon;

SprintIcon.propTypes = {
  sprintType: PropTypes.string.isRequired,
};
