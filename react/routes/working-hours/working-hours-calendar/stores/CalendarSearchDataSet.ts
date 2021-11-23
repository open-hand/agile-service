import { DataSet } from 'choerodon-ui/pro';
import moment, { Moment } from 'moment';
import { debounce } from 'lodash';
import { DataSetSelection } from 'choerodon-ui/pro/lib/data-set/enum';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization, getOrganizationId } from '@/utils/common';

export const formatStartDate = (date: string | Moment, format = false) => {
  if (!date) {
    return undefined;
  }
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

interface WorkGroupItem {
  parentId: string | null | number,
  id: string | null
}
const searchDsUpdate = ({
  // @ts-ignore
  name, value,
}, projectCreationDate: any) => {
  if (name === 'startTime') {
    const adjustedStartDate = getIsOrganization() ? formatStartDate(moment().subtract(6, 'days'), true) : formatStartDate(moment().subtract(6, 'days').isBefore(moment(projectCreationDate)) ? moment(projectCreationDate) : moment().subtract(6, 'days'), true);
    if (value) {
      const formateStartDate = formatStartDate(value, true) || adjustedStartDate;
      localPageCacheStore.setItem('workingHours-calendar-startTime', formateStartDate);
    } else {
      localPageCacheStore.setItem('workingHours-calendar-startTime', adjustedStartDate);
    }
  }

  if (name === 'endTime') {
    if (value) {
      const formateEndDate = formatEndDate(value, true) || formatEndDate(moment(), true);
      localPageCacheStore.setItem('workingHours-calendar-endTime', formateEndDate);
    } else {
      localPageCacheStore.setItem('workingHours-calendar-endTime', formatEndDate(moment(), true));
    }
  }

  if (name === 'userIds' || name === 'projectIds' || name === 'workGroupIds') {
    localPageCacheStore.setItem(`workingHours-calendar-${name}`, value);
  }
};

const LogSearchDataSet = ({ projectCreationDate, cacheFiltersObj }: { projectCreationDate: string | undefined, cacheFiltersObj?: { userIds?: string[], workGroupIds?: string[] }}) => ({
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
        moment(record?.get('endTime')).subtract(31, 'days').isAfter(moment(projectCreationDate))) ? (formatStartDate(moment(record?.get('endTime'))) as Moment).subtract(31, 'days') : moment(projectCreationDate).startOf('day')
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
  }, {
    name: 'workGroupIds',
    multiple: true,
    textField: 'name',
    valueField: 'id',
    options: new DataSet({
      selection: 'single' as DataSetSelection,
      autoQuery: !!getIsOrganization(),
      idField: 'id',
      parentField: 'parentId',
      transport: {
        read: () => ({
          url: `/agile/v1/organizations/${getOrganizationId()}/work_group/query_tree`,
          method: 'get',
          transformResponse: (res) => {
            try {
              const data = JSON.parse(res);
              if (data && data.workGroupVOS) {
                const removeOrgItem = data.workGroupVOS.filter((item: WorkGroupItem) => !(item.parentId === null && item.id === null));
                return removeOrgItem.map((item: WorkGroupItem) => {
                  if (item.id === null && item.parentId === 0) {
                    return { ...item, id: '0' };
                  }
                  return item;
                });
              }
              return data;
            } catch (error) {
              return res;
            }
          },
        }),
      },
    }),
  }],
  data: [{
    endTime: localPageCacheStore.getItem('workingHours-calendar-endTime') ? moment(localPageCacheStore.getItem('workingHours-calendar-endTime')) : formatEndDate(moment()),
    // eslint-disable-next-line no-nested-ternary
    startTime: localPageCacheStore.getItem('workingHours-calendar-startTime') ? moment(localPageCacheStore.getItem('workingHours-calendar-startTime')) : formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(projectCreationDate)) ? moment(projectCreationDate) : moment().subtract(6, 'days')
    )),
    userIds: cacheFiltersObj?.userIds?.length ? cacheFiltersObj?.userIds : localPageCacheStore.getItem('workingHours-calendar-userIds'),
    projectIds: localPageCacheStore.getItem('workingHours-calendar-projectIds'),
    workGroupIds: cacheFiltersObj?.workGroupIds?.length ? cacheFiltersObj?.workGroupIds : localPageCacheStore.getItem('workingHours-calendar-workGroupIds'),
  }],
  events: {
    update: debounce((updateData: any) => {
      searchDsUpdate(updateData, projectCreationDate);
    }, 500),
  },
});

export default LogSearchDataSet;
