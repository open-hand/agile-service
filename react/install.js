import { set } from '@choerodon/inject';
import { loadCustomReportData } from '@/injects/custom-report/inject';
import openTemplate from './injects/template-modal';
import to from '@/utils/to';

set('agile:openTemplate', openTemplate);
set('agile:SelectSprint', () => import('@/components/select/select-sprint'));
set('agile:SelectVersion', () => import('@/components/select/select-version'));
set('agile:PersonalWorkload', () => import('@/injects/personal-workload'));
set('agile:AgileChartHeaderButtons', () => import('@/injects/custom-report/header-buttons'));
set('agile:AgileCustomChartLoadData', loadCustomReportData);
set('agile:CustomChartSearch', () => import('@/injects/custom-report/custom-search'));
set('agile:SelectUser', () => import('@/components/select/select-user'));
set('agile:WorkbenchGantt', () => import('@/injects/workbench-gantt'));
set('agile:ChunkUploader', () => import('@/components/chunk-uploader'));
set('agile:Preview', () => import('@/components/Preview'));
set('agile:to', to);
