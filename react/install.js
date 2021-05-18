import { set } from '@choerodon/inject';
import openTemplate from './injects/template-modal';

set('agile:openTemplate', openTemplate);
set('agile:SelectSprint', () => import('@/components/select/select-sprint'));
set('agile:SelectVersion', () => import('@/components/select/select-version'));
set('agile:PersonalWorkload', () => import('@/injects/personal-workload'));
