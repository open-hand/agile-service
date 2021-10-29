import { DataSet } from 'choerodon-ui/pro';
import moment, { Moment } from 'moment';
import { debounce, includes } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization } from '@/utils/common';

export const formatStartDate = (date: string | Moment, format = false) => {
  if (!format) {
    return moment(date).startOf('day');
  }
  return moment(date).startOf('day').format('YYYY-MM-DD HH:mm:ss');
};

export const formatEndDate = (date: string | Moment, format = false) => {
  if (!format) {
    return moment(date).endOf('day');
  }
  return moment(date).endOf('day').format('YYYY-MM-DD HH:mm:ss');
};

const searchDsUpdate = ({
  // @ts-ignore
  name, value,
}, calendarDs: DataSet, currentProject: any) => {
  if (name === 'startTime') {
    const adjustedStartDate = getIsOrganization() ? formatStartDate(moment().subtract(6, 'days'), true) : formatStartDate(moment().subtract(6, 'days').isBefore(moment(currentProject?.creationDate)) ? moment(currentProject?.creationDate) : moment().subtract(6, 'days'), true);
    if (value) {
      const formateStartDate = formatStartDate(value, true) || adjustedStartDate;
      localPageCacheStore.setItem('workingHours-calendar-startTime', formateStartDate);
      calendarDs.setQueryParameter('startTime', formateStartDate);
    } else {
      localPageCacheStore.setItem('workingHours-calendar-startTime', adjustedStartDate);
      calendarDs.setQueryParameter('startTime', adjustedStartDate);
    }
  }

  if (name === 'endTime') {
    if (value) {
      const formateEndDate = formatEndDate(value, true) || formatEndDate(moment(), true);
      localPageCacheStore.setItem('workingHours-calendar-endTime', formateEndDate);
      calendarDs.setQueryParameter('endTime', formateEndDate);
    } else {
      localPageCacheStore.setItem('workingHours-calendar-endTime', formatEndDate(moment(), true));
      calendarDs.setQueryParameter('endTime', formatEndDate(moment()));
    }
  }

  if (name === 'userIds') {
    localPageCacheStore.setItem('workingHours-calendar-userIds', value);
    calendarDs.setQueryParameter('userIds', value);
  }
  if (name === 'projectIds') {
    localPageCacheStore.setItem('workingHours-calendar-projectIds', value);
    calendarDs.setQueryParameter('projectIds', value);
  }
  calendarDs.query();
};

const LogSearchDataSet = ({ calendarDs, currentProject }: { calendarDs: DataSet, currentProject: any}) => ({
  autoCreate: true,
  autoQuery: false,
  fields: [{
    name: 'projectIds',
    textField: 'name',
    valueField: 'id',
  }, {
    name: 'startTime',
    required: true,
    dynamicProps: {
      max: ({ record }: { record: Record}) => moment(record.get('endTime')).startOf('day'),
      // eslint-disable-next-line no-nested-ternary
      min: ({ record }: { record: Record }) => (getIsOrganization() ? formatStartDate(moment(record?.get('endTime')).subtract(31, 'days')) : (
        moment(record?.get('endTime')).subtract(31, 'days').isAfter(moment(currentProject?.creationDate))) ? (formatStartDate(moment(record?.get('endTime'))) as Moment).subtract(31, 'days') : moment(currentProject?.creationDate).startOf('day')
      ),
    },
  }, {
    name: 'endTime',
    required: true,
    dynamicProps: {
      max: ({ record }: { record: Record}) => (moment(record?.get('startTime')).add(31, 'days').isBefore(moment()) ? (formatStartDate(moment(record?.get('startTime'))) as Moment).add(31, 'days').endOf('day') : moment().endOf('day')),
      min: ({ record }: { record: Record }) => moment(record.get('startTime')).startOf('day'),
    },
  }, {
    name: 'userIds',
    multiple: true,
    textField: 'realName',
    valueField: 'id',
  }],
  data: [{
    endTime: localPageCacheStore.getItem('workingHours-calendar-endTime') ? moment(localPageCacheStore.getItem('workingHours-calendar-endTime')) : formatEndDate(moment()),
    // eslint-disable-next-line no-nested-ternary
    startTime: localPageCacheStore.getItem('workingHours-calendar-startTime') ? moment(localPageCacheStore.getItem('workingHours-calendar-startTime')) : formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(currentProject?.creationDate)) ? moment(currentProject?.creationDate) : moment().subtract(6, 'days')
    )),
    userIds: localPageCacheStore.getItem('workingHours-calendar-userIds'),
  }],
  events: {
    update: debounce((updateData: any) => {
      searchDsUpdate(updateData, calendarDs, currentProject);
    }, 500),
  },
});

export default LogSearchDataSet;
