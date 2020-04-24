import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Choerodon } from '@choerodon/boot';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { getAllPIList, changeIssuePI } from '@/api/PIApi';
import TextEditToggle from '@/components/TextEditToggle';

const { Text, Edit } = TextEditToggle;
@observer
class FieldPI extends Component {
  updateIssuePI = async (value, done) => {
    const {
      store, onUpdate, reloadIssue, 
    } = this.props;
    const issue = store.getIssue;
    const { issueId, activePi } = issue;
    const { id } = activePi || {};
    await changeIssuePI(issueId, id, value || 0);    
    if (onUpdate) {
      onUpdate();
    }
    if (reloadIssue) {
      reloadIssue(issueId);
    }
  }


  render() {
    const { store, hasPermission } = this.props;
    const issue = store.getIssue;
    const { activePi, closePi } = issue;
    const {
      name, code, id, statusCode,
    } = activePi || {};
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            PI
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={!hasPermission && statusCode === 'doing'}
            formKey="pi"
            onSubmit={this.updateIssuePI}
            originData={id}
          >
            <Text>
              {closePi.length > 0 ? `历史PI:${closePi.map(p => `${p.code}-${p.name}`)}` : ''}
              {`活跃PI:${name ? `${code}-${name}` : '无'}`}
            </Text>
            <Edit>
              <SelectFocusLoad
                allowClear
                type="all_pi"
                request={() => getAllPIList(['todo', 'doing'])}
              />
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldPI;
