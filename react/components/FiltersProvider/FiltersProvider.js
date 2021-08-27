import React from 'react';
import PropTypes from 'prop-types';
import {
  userApi, componentApi, epicApi, versionApi, issueTypeApi, issueLabelApi, priorityApi, statusApi,
  sprintApi,
} from '@/api';

const requests = {
  issueType: {
    textField: 'name',
    valueField: 'id',
    request: issueTypeApi.loadAllWithStateMachineId.bind(issueTypeApi),
  },
  issueStatus: {
    textField: 'name',
    valueField: 'id',
    request: statusApi.loadByProject.bind(statusApi),
  },
  priority: {
    textField: 'name',
    valueField: 'id',
    request: priorityApi.loadByProject.bind(priorityApi),
  },
  user: {
    textField: 'name',
    valueField: 'id',
    request: userApi.getAllInProject.bind(userApi),
  },
  sprint: {
    textField: 'sprintName',
    valueField: 'sprintId',
    request: sprintApi.loadSprints,
  },
  version: {
    textField: 'name',
    valueField: 'versionId',
    request: versionApi.loadNamesByStatus.bind(versionApi),
  },
  label: {
    textField: 'labelName',
    valueField: 'labelId',
    request: () => issueLabelApi.loads(),
  },
  component: {
    isContent: true,
    textField: 'name',
    valueField: 'componentId',
    request: componentApi.loadAllComponents,
  },
  epic: {
    textField: 'epicName',
    valueField: 'issueId',
    request: () => epicApi.loadEpicsForSelect(undefined, { size: 0 }),
  },
};

function transform(type, data) {
  const {
    isContent, textField, valueField, formatter,
  } = requests[type];
  const list = isContent ? data.list : data;
  if (formatter) {
    return list.map(formatter);
  }
  return list.map((item) => ({
    text: item[textField],
    value: item[valueField].toString(),
  }));
}

const FiltersProviderHOC = (fields = []) => (Component) => class FiltersProvider extends React.Component {
  constructor() {
    super();
    const filters = {};
    fields.forEach((field) => {
      if (typeof field === 'string') {
        filters[field] = [];
      } else {
        filters[field.key] = [];
      }
    });
    this.state = {
      filters,
    };
  }

  componentDidMount() {
    const keys = fields.map((field) => (typeof field === 'string' ? field : field.key));
    const args = fields.map((field) => (typeof field === 'string' ? undefined : field.args));

    const requestQueue = keys.map((key, i) => requests[key].request.apply(null, args[i]));
    Promise.all(requestQueue).then((values) => {
      const filters = {};
      values.forEach((value, i) => {
        filters[keys[i]] = transform(keys[i], value);
        transform(keys[i], value);
      });
      this.setState({
        filters,
      });
    });
  }

  render() {
    const { filters } = this.state;
    return <Component {...this.props} filters={filters} />;
  }
};

export default FiltersProviderHOC;
