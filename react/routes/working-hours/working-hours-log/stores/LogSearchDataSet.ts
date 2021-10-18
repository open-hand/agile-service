import { DataSet } from 'choerodon-ui/pro';
import moment from 'moment';
import { debounce, includes } from 'lodash';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

const searchDsUpdate = ({
  // @ts-ignore
  name, value,
}, logDs: DataSet, currentProject: any) => {
  if (name === 'dateRange') {
    if (value) {
      const formateStartDate = (value[0] && value[0].startOf('day').format('YYYY-MM-DD HH:mm:ss')) || moment().subtract(31, 'days');
      const formateEndDate = (value[1] && value[1].endOf('day').format('YYYY-MM-DD HH:mm:ss')) || moment();
      localPageCacheStore.setItem('workingHours-log-startTime', formateStartDate);
      localPageCacheStore.setItem('workingHours-log-endTime', formateEndDate);
      logDs.setQueryParameter('startTime', formateStartDate);
      logDs.setQueryParameter('endTime', formateEndDate);
    } else {
      localPageCacheStore.setItem('workingHours-log-startTime', moment().subtract(31, 'days'));
      localPageCacheStore.setItem('workingHours-log-endTime', moment());
      logDs.setQueryParameter('startTime', moment().subtract(31, 'days'));
      logDs.setQueryParameter('endTime', moment());
    }
  }
  if (name === 'userIds') {
    localPageCacheStore.setItem('workingHours-log-userIds', value);
    logDs.setQueryParameter('userIds', value);
  }
  if (name === 'projectIds') {
    localPageCacheStore.setItem('workingHours-log-projectIds', value);
    logDs.setQueryParameter('projectIds', value);
  }
  logDs.query();
};

const LogSearchDataSet = ({ logDs, currentProject }: { logDs: DataSet, currentProject: any}) => ({
  autoCreate: true,
  autoQuery: false,
  fields: [{
    name: 'projectIds',
    textField: 'name',
    valueField: 'id',
  }, {
    name: 'dateRange',
    range: true,
    required: true,
    max: moment().endOf('day'),
    min: moment(currentProject?.creationDate).startOf('day'),
  }, {
    name: 'userIds',
    multiple: true,
    textField: 'realName',
    valueField: 'id',
  }],
  data: [{
    dateRange: [localPageCacheStore.getItem('workingHours-log-startTime') ? moment(localPageCacheStore.getItem('workingHours-log-startTime')) : moment().subtract(31, 'days'), localPageCacheStore.getItem('workingHours-log-endTime') ? moment(localPageCacheStore.getItem('workingHours-log-endTime')) : moment()],
    userIds: localPageCacheStore.getItem('workingHours-log-userIds'),
  }],
  events: {
    update: debounce((updateData: any) => {
      searchDsUpdate(updateData, logDs, currentProject);
    }, 500),
  },
});

export default LogSearchDataSet;
