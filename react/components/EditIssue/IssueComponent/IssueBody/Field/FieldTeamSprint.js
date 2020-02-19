import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import _ from 'lodash';
import TextEditToggle from '../../../../TextEditToggle';

const { Text, Edit } = TextEditToggle;

@inject('AppState')
@observer class FieldTeamSprint extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  componentDidMount() {
  }

  render() {
    const { store } = this.props;
    const issue = store.getIssue;
    const { teamSprint = [] } = issue;
    const teamSprints = _.map(teamSprint, 'sprints');
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            团队冲刺
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled
            // formKey="teamSprint"
            originData={_.uniq(_.map(_.flatten(teamSprints), 'sprintId'))}
          >
            <Text>
              {
                teamSprints.length > 0 ? (
                  <div>
                    {
                      _.map(teamSprints, sprintArr => _.map(sprintArr, 'sprintName')).join(',')
                    }
                  </div>
                ) : (
                  <div>
                    无
                  </div>
                )
              }
            </Text>
            <Edit>
              <span>{_.map(teamSprints, sprintArr => _.map(sprintArr, 'sprintName')).join(',')}</span>
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldTeamSprint));
