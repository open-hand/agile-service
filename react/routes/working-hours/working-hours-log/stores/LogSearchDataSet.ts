import { DataSet } from 'choerodon-ui/pro';
import moment from 'moment';
import { debounce } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization } from '@/utils/common';
import { formatEndDate, formatStartDate } from '../../utils';

const searchDsUpdate = ({
  // @ts-ignore
  name, value, record,
}, logDs: DataSet) => {
  if (name === 'startTime') {
    if (value) {
      const formateStartDate = formatStartDate(value, true);
      localPageCacheStore.setItem('workingHours-log-startTime', formateStartDate);
      logDs.setQueryParameter('startTime', formateStartDate);

      const endTime = record.get('endTime');
      if (moment(endTime).endOf('day').diff(value, 'days') > 31) {
        const newEndTime = moment(value).add(31, 'days');
        record.set('endTime', newEndTime);
        localPageCacheStore.setItem('workingHours-log-endTime', newEndTime);
        logDs.setQueryParameter('endTime', formatEndDate(newEndTime, true));
        return;
      }
    }
  }

  if (name === 'endTime') {
    if (value) {
      const formateEndDate = formatEndDate(value, true);
      localPageCacheStore.setItem('workingHours-log-endTime', formateEndDate);
      logDs.setQueryParameter('endTime', formateEndDate);

      const startTime = record.get('startTime');
      if (moment(value).endOf('day').diff(startTime, 'days') > 31) {
        const newStartTime = moment(value).subtract(31, 'days');
        record.set('startTime', newStartTime);
        localPageCacheStore.setItem('workingHours-log-startTime', newStartTime);
        logDs.setQueryParameter('startTime', formatStartDate(newStartTime, true));
        return;
      }
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

const LogSearchDataSet = ({ logDs, projectCreationDate }: { logDs: DataSet, projectCreationDate: string}) => ({
  autoCreate: true,
  autoQuery: false,
  fields: [{
    name: 'projectIds',
    textField: 'name',
    valueField: 'id',
  }, {
    name: 'startTime',
    required: true,
    min: getIsOrganization() ? undefined : formatStartDate(projectCreationDate),
    dynamicProps: {
      max: ({ record }: { record: Record}) => moment(record.get('endTime')).startOf('day'),
    },
  }, {
    name: 'endTime',
    required: true,
    max: moment().endOf('day'),
    dynamicProps: {
      min: ({ record }: { record: Record }) => moment(record.get('startTime')).startOf('day'),
    },
  }, {
    name: 'userIds',
    multiple: true,
    textField: 'realName',
    valueField: 'id',
  }],
  data: [{
    endTime: localPageCacheStore.getItem('workingHours-log-endTime') ? moment(localPageCacheStore.getItem('workingHours-log-endTime')) : formatEndDate(moment()),
    // eslint-disable-next-line no-nested-ternary
    startTime: localPageCacheStore.getItem('workingHours-log-startTime') ? moment(localPageCacheStore.getItem('workingHours-log-startTime')) : formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(projectCreationDate)) ? moment(projectCreationDate) : moment().subtract(6, 'days')
    )),
    userIds: localPageCacheStore.getItem('workingHours-log-userIds'),
  }],
  events: {
    update: debounce((updateData: any) => {
      searchDsUpdate(updateData, logDs);
    }, 500),
  },
});

export default LogSearchDataSet;
