import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui/pro';
import { difference, map } from 'lodash';
import { featureApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectTeamSprints from '@/components/select/select-teamSprint';

@observer class FieldProgramSprint extends Component {
  updateIssueSprint = async (sprintIds) => {
    const {
      store, onUpdate, reloadIssue, setIssueLoading,
    } = this.props;
    const issue = store.getIssue;
    const { issueId } = issue;
    const activePiSprints = issue.activePiSprints || [];
    const previousSprintIds = activePiSprints.map((s) => s.sprintId);
    const isEqualValueArray = difference(previousSprintIds, sprintIds || []).length === 0;
    if (isEqualValueArray) {
      return;
    }
    const originSprintIds = activePiSprints.map((sprint) => sprint.sprintId);
    const addSprints = (sprintIds || []).filter((teamId) => !originSprintIds.includes(teamId));
    const removeSprints = sprintIds ? originSprintIds.filter(
      (sprintId) => !sprintIds.includes(sprintId),
    ) : originSprintIds;
    setIssueLoading(true);
    await featureApi.updateTeamAndSprint({
      piId: issue.activePi ? issue.activePi.id : null,
      deleteSprintIds: removeSprints,
      featureId: issue.issueId,
      sprintIds: addSprints,
      teamProjectIds: [],
      deleteTeamProjectIds: [],
    });

    if (onUpdate) {
      onUpdate();
    }
    await reloadIssue(issueId);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const activePiSprints = issue.activePiSprints || [];
    const closedPiSprints = issue.closedPiSprints || [];
    const sprintIds = activePiSprints.map((s) => s.sprintId);
    const activePiTeams = issue.activePiTeams || [];
    const { id: piId } = issue.activePi || {};
    const teamIds = activePiTeams.map((team) => team.id);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            冲刺
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            formKey="sprint"
            onSubmit={this.updateIssueSprint}
            initValue={sprintIds}
            editor={({ submit }) => (
              <SelectTeamSprints
                label="活跃冲刺"
                mode="multiple"
                projectId={store.projectId}
                onBlur={submit}
                // getPopupContainer={() => document.getElementById('detail')}
                allowClear
                showCheckAll={false}
                piId={piId}
                teamIds={teamIds}
              />
            )}
            editExtraContent={
              closedPiSprints.length ? (
                <div style={{ maxWidth: 170 }}>
                  <span>已结束冲刺</span>
                  <span>
                    {map(closedPiSprints, 'sprintName').join(' , ')}
                  </span>
                </div>
              ) : null
            }
          >
            <Tooltip
              placement="top"
              title={`该特性经历迭代数${closedPiSprints.length + activePiSprints.length}`}
            >
              <div>
                {
                  closedPiSprints.concat(activePiSprints).length === 0 ? '无' : (
                    <div>
                      <div>
                        {map(closedPiSprints, 'sprintName').join(' , ')}
                      </div>
                      <div>
                        {activePiSprints.map((sprint) => (
                          <div
                            style={{
                              color: '#4d90fe',
                              fontSize: '13px',
                              lineHeight: '20px',
                              marginTop: closedPiSprints.length ? 5 : 0,
                            }}
                          >
                            {sprint.sprintName}
                          </div>
                        ))}
                      </div>
                    </div>
                  )
                }
              </div>
            </Tooltip>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldProgramSprint;
