import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { workingHoursApiConfig } from '@/api';

const CalendarDataSet = (): DataSetProps => ({
  autoQuery: false,
  paging: true,
  selection: false,
  pageSize: 20,
  transport: {
    read: ({ params, data }) => workingHoursApiConfig.getCalendar({ params, data }),
  },
  fields: [],
});

export default CalendarDataSet;
