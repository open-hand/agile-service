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

const LogExportDataSet = ({ currentProject }: { currentProject: any}) => ({
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
    dynamicProps: {
      max: ({ record }: { record: Record}) => moment(record.get('endTime')).startOf('day'),
      // eslint-disable-next-line no-nested-ternary
      min: ({ record }: { record: Record }) => (getIsOrganization() ? formatStartDate(moment(record?.get('endTime')).subtract(31, 'days')) : (
        moment(record?.get('endTime')).subtract(31, 'days').isAfter(moment(currentProject?.creationDate))) ? (formatStartDate(moment(record?.get('endTime'))) as Moment).subtract(31, 'days') : moment(currentProject?.creationDate).startOf('day')
      ),
    },
    label: '开始时间',
  }, {
    name: 'endTime',
    required: true,
    dynamicProps: {
      max: ({ record }: { record: Record}) => (moment(record?.get('startTime')).add(31, 'days').isBefore(moment()) ? (formatStartDate(moment(record?.get('startTime'))) as Moment).add(31, 'days') : moment().endOf('day')),
      min: ({ record }: { record: Record }) => moment(record.get('startTime')).startOf('day'),
    },
    label: '结束时间',
  }, {
    name: 'userIds',
    multiple: true,
    textField: 'realName',
    valueField: 'id',
    label: '筛选成员',
  }],
});

export default LogExportDataSet;
