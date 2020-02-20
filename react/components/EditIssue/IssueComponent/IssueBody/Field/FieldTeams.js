import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import _ from 'lodash';
import { hexToRgba } from '../../../../../common/utils';
import TextEditToggle from '../../../../TextEditToggle';

const { Text, Edit } = TextEditToggle;

@inject('AppState')
@observer class FieldTeams extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  componentDidMount() {
  }

  render() {
    const { store, TEAMSCOLOR } = this.props;
    const issue = store.getIssue;
    const { teamSprint = [] } = issue;
    const teams = _.map(teamSprint, 'projectVO');
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            负责团队
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled
            // formKey="teams"
            originData={_.map(teams, 'id')}
          >
            <Text>
              {
                teams.length > 0 ? (
                  <div style={{
                    display: 'flex',
                    flexWrap: 'wrap',
                  }}
                  >
                    {
                      teams.map((team, i) => (
                        <div style={{ 
                          color: `${TEAMSCOLOR[i % 6]}`, 
                          border: `1px solid ${TEAMSCOLOR[i % 6]}`, 
                          background: `${hexToRgba(TEAMSCOLOR[i % 6], 0.05)}`,
                          marginRight: 2,
                          marginBottom: 3,
                          padding: '1px 2px',
                          display: 'block',
                          overflow: 'hidden',
                          textOverflow: 'ellipsis',
                          whiteSpace: 'noWrap',
                        }}
                        >
                          {team.name}
                        </div>
                      ))
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
              <span>{_.map(teams, 'name').join(',')}</span>
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldTeams));
