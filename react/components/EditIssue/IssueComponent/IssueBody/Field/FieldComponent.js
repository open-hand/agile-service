import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import _ from 'lodash';
import { issueApi } from '@/api';
import TextEditToggle from '../../../../TextEditToggle';
import SelectFocusLoad from '../../../../SelectFocusLoad';

const { Text, Edit } = TextEditToggle;

@inject('AppState')
@observer class FieldComponent extends Component {
  transToArr = (arr, pro, type = 'string') => {
    if (!arr.length) {
      return type === 'string' ? '无' : [];
    } else if (typeof arr[0] === 'object') {
      return type === 'string' ? _.map(arr, pro).join() : _.map(arr, pro);
    } else {
      return type === 'string' ? arr.join() : arr;
    }
  };

  updateIssueComponents = (newComponents) => {
    const {
      store, onUpdate, reloadIssue, AppState,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const componentIssueRelVOList = [];
    newComponents.forEach((label) => {
      const target = _.find(this.componentList, { name: label });
      if (target) {
        componentIssueRelVOList.push(target);
      } else {
        componentIssueRelVOList.push({
          name: label,
          projectId: AppState.currentMenuType.id,
        });
      }
    });
    const obj = {
      issueId,
      objectVersionNumber,
      componentIssueRelVOList,
    };
    issueApi.update(obj)
      .then(() => {
        if (onUpdate) {
          onUpdate();
        }
        if (reloadIssue) {
          reloadIssue(issueId);
        }
      });
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { componentIssueRelVOList = [] } = issue;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            模块
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            formKey="component"
            onSubmit={this.updateIssueComponents}
            originData={componentIssueRelVOList.map(component => component.name)}
          >
            <Text>
              {componentIssueRelVOList && componentIssueRelVOList.length
                ? (
                  <div>
                    <p className="primary" style={{ wordBreak: 'break-word', marginTop: 2 }}>
                      {this.transToArr(componentIssueRelVOList, 'name')}
                    </p>
                  </div>
                ) : (
                  <div>
                    无
                  </div>
                )
              }
            </Text>
            <Edit>
              <SelectFocusLoad
                type="component"
                dropdownMatchSelectWidth={false}
                mode="multiple"
                getPopupContainer={() => document.getElementById('detail')} 
                saveList={(componentList) => { this.componentList = componentList; }}
                style={{ marginTop: 0, paddingTop: 0 }}
              />
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldComponent));
