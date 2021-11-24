import moment, { Moment } from 'moment';
import { DataSet } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { DataSetSelection, FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

import { getIsOrganization, getOrganizationId } from '@/utils/common';
import { formatStartDate } from '../../utils';

interface WorkGroupItem {
  parentId: string | null | number,
  id: string | null
}

const LogExportDataSet = ({ projectCreationDate, showWorkGroup = false }: { projectCreationDate: string | undefined, showWorkGroup?: boolean}): DataSetProps => ({
  autoCreate: true,
  autoQuery: false,
  fields: [{
    name: 'projectIds',
    textField: 'name',
    valueField: 'id',
    label: '所属项目',
  }, {
    name: 'startTime',
    required: true,
    min: getIsOrganization() ? undefined : formatStartDate(projectCreationDate),
    dynamicProps: {
      max: ({ record }: { record: Record}) => moment(record.get('endTime')).startOf('day'),
    },
    label: '开始时间',
  }, {
    name: 'endTime',
    required: true,
    max: moment().endOf('day'),
    dynamicProps: {
      min: ({ record }: { record: Record }) => moment(record.get('startTime')).startOf('day'),
    },
    label: '结束时间',
  }, {
    name: 'userIds',
    multiple: true,
    textField: 'realName',
    valueField: 'id',
    label: '筛选成员',
  }, {
    name: 'workGroupIds',
    multiple: true,
    textField: 'name',
    valueField: 'id',
    label: '筛选工作组',
    options: new DataSet({
      selection: 'single' as DataSetSelection,
      autoQuery: showWorkGroup && !!getIsOrganization(),
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
  }, {
    name: 'exportMonthlyReport',
    label: '按工作组统计总量',
    type: 'boolean' as FieldType,
  }],
  events: {
    update: ({ name, value, record }: { name: string, value: any, record: Record}) => {
      if (name === 'startTime') {
        if (value) {
          const endTime = record.get('endTime');
          if (moment(endTime).endOf('day').diff(value, 'days') > 31) {
            const newEndTime = moment(value).add(31, 'days');
            record.set('endTime', newEndTime);
            return;
          }
        }
      }

      if (name === 'endTime') {
        if (value) {
          const startTime = record.get('startTime');
          if (moment(value).endOf('day').diff(startTime, 'days') > 31) {
            const newStartTime = moment(value).subtract(31, 'days');
            record.set('startTime', newStartTime);
          }
        }
      }
    },
  },
});

export default LogExportDataSet;
