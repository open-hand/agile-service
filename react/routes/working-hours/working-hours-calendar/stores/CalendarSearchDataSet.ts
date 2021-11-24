import { DataSet } from 'choerodon-ui/pro';
import moment from 'moment';
import { debounce } from 'lodash';
import { DataSetSelection } from 'choerodon-ui/pro/lib/data-set/enum';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization, getOrganizationId } from '@/utils/common';
import { formatEndDate, formatStartDate } from '../../utils';

interface WorkGroupItem {
  parentId: string | null | number,
  id: string | null
}
const searchDsUpdate = ({
  // @ts-ignore
  name, value, record,
}) => {
  if (name === 'startTime') {
    if (value) {
      const formateStartDate = formatStartDate(value, true);
      localPageCacheStore.setItem('workingHours-calendar-startTime', formateStartDate);

      const endTime = record.get('endTime');
      if (moment(endTime).endOf('day').diff(value, 'days') > 31) {
        const newEndTime = moment(value).add(31, 'days');
        record.set('endTime', newEndTime);
        localPageCacheStore.setItem('workingHours-calendar-endTime', newEndTime);
        return;
      }
    }
  }

  if (name === 'endTime') {
    if (value) {
      const formateEndDate = formatEndDate(value, true);
      localPageCacheStore.setItem('workingHours-calendar-endTime', formateEndDate);

      const startTime = record.get('startTime');
      if (moment(value).endOf('day').diff(startTime, 'days') > 31) {
        const newStartTime = moment(value).subtract(31, 'days');
        record.set('startTime', newStartTime);
        localPageCacheStore.setItem('workingHours-calendar-startTime', newStartTime);
        return;
      }
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
          url: `/agile/v1/organizations/${getOrganizationId()}/work_bench/work_group/query_tree`,
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
      searchDsUpdate(updateData);
    }, 500),
  },
});

export default LogSearchDataSet;
