import moment, { Moment } from 'moment';
import { debounce } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization } from '@/utils/common';
import { formatEndDate, formatStartDate } from '../../utils';

const searchDsUpdate = ({
  // @ts-ignore
  name, value, record,
}) => {
  if (name === 'startTime') {
    if (value) {
      const formateStartDate = formatStartDate(value, true);
      localPageCacheStore.setItem('workingHours-issue-startTime', formateStartDate);

      const endTime = record.get('endTime');
      if (moment(endTime).endOf('day').diff(value, 'days') > 31) {
        const newEndTime = moment(value).add(31, 'days');
        record.set('endTime', newEndTime);
        localPageCacheStore.setItem('workingHours-issue-endTime', newEndTime);
        return;
      }
    }
  }

  if (name === 'endTime') {
    if (value) {
      const formateEndDate = formatEndDate(value, true);
      localPageCacheStore.setItem('workingHours-issue-endTime', formateEndDate);

      const startTime = record.get('startTime');
      if (moment(value).endOf('day').diff(startTime, 'days') > 31) {
        const newStartTime = moment(value).subtract(31, 'days');
        record.set('startTime', newStartTime);
        localPageCacheStore.setItem('workingHours-issue-startTime', newStartTime);
      }
    }
  }
};

const DateSearchDataSet = ({ projectCreationDate }: { projectCreationDate: string}) => ({
  autoCreate: true,
  autoQuery: false,
  fields: [{
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
  }],
  data: [{
    endTime: localPageCacheStore.getItem('workingHours-issue-endTime') ? moment(localPageCacheStore.getItem('workingHours-issue-endTime')) : formatEndDate(moment()),
    // eslint-disable-next-line no-nested-ternary
    startTime: localPageCacheStore.getItem('workingHours-issue-startTime') ? moment(localPageCacheStore.getItem('workingHours-issue-startTime')) : formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(projectCreationDate)) ? moment(projectCreationDate) : moment().subtract(6, 'days')
    )),
    userIds: localPageCacheStore.getItem('workingHours-issue-userIds'),
  }],
  events: {
    update: debounce((updateData: any) => {
      searchDsUpdate(updateData);
    }, 500),
  },
});

export default DateSearchDataSet;
