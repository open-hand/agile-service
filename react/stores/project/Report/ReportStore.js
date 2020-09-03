import { observable, action, computed } from 'mobx';
import { store } from '@choerodon/boot';
import { sprintApi } from '@/api';

@store('ReportStore')
class ReportStore {
  @observable loading = false;

  // @observable todo = false;

  // @observable done = false;

  // @observable remove = false;

  @observable sprints = [];

  @observable currentSprint = {};

  @observable activeKey = 'done';

  @observable doneIssues = [];

  @observable todoIssues = [];

  @observable removeIssues = [];

  @observable chartData = {
    xAxis: [],
    yAxis: [],
  };

  @observable donePagination = {
    current: 1,
    pageSize: 10,
    total: undefined,
  };

  @observable todoPagination = {
    current: 1,
    pageSize: 10,
    total: undefined,
  };

  @observable removePagination = {
    current: 1,
    pageSize: 10,
    total: undefined,
  }

  init() {
    sprintApi.loadSprints(['started', 'closed'])
      .then((res) => {
        this.setSprints(res || []);
        if (res && res.length) {
          this.changeCurrentSprint(res[0].sprintId);
        } else {
          this.setCurrentSprint({});
        }
      })
      .catch((error) => {
        this.setSprints([]);
      });
  }

  changeCurrentSprint(sprintId) {
    if (sprintId) {
      sprintApi.loadSprint(sprintId)
        .then((res) => {
          this.setCurrentSprint(res || {});
          this.loadCurrentTab();
        })
        .catch((error) => {
        });
    } else {
      this.init();
    }
  }

  loadCurrentTab() {
    const ARRAY = {
      done: 'loadDoneIssues',
      todo: 'loadTodoIssues',
      remove: 'loadRemoveIssues',
    };
    if (!this.currentSprint.sprintId) {
      return;
    }
    this[ARRAY[this.activeKey]]();
  }

  loadDoneIssues(page = 1, size = 10) {
    this.setLoading(true);
    sprintApi.loadSprintIssues(this.currentSprint.sprintId, 'done', page, size)
      .then((res) => {
        this.setDoneIssues(res.list);
        this.setDonePagination({
          ...this.donePagination,
          total: res.total,
        });
        this.setLoading(false);
        // this.setDone(true);
      });
  }

  loadTodoIssues(page = 1, size = 10) {
    this.setLoading(true);
    sprintApi.loadSprintIssues(this.currentSprint.sprintId, 'unfinished', page, size)
      .then((res) => {
        this.setTodoIssues(res.list);
        this.setTodoPagination({
          ...this.todoPagination,
          total: res.total,
        });
        this.setLoading(false);
        // this.setTodo(true);
      });
  }

  loadRemoveIssues(page = 0, size = 10) {
    this.setLoading(true);
    sprintApi.loadSprintIssues(this.currentSprint.sprintId, 'remove', page, size)
      .then((res) => {
        this.setRemoveIssues(res.list);
        this.setRemovePagination({
          ...this.removePagination,
          total: res.total,
        });
        this.setLoading(false);
        // this.setRemove(true);
      });
  }

  @action setSprints(data) {
    this.sprints = data;
  }

  @action setCurrentSprint(data) {
    this.currentSprint = data;
  }

  @action setActiveKey(data) {
    this.activeKey = data;
  }

  @action setPagination(data) {
    this.pagination = data;
  }

  @action setFilter(data) {
    this.filter = data;
  }

  @action setOrder(data) {
    this.order = data;
  }

  @action setLoading(data) {
    this.loading = data;
  }

  @action setDone(data) {
    this.done = data;
  }

  @action setTodo(data) {
    this.todo = data;
  }

  @action setRemove(data) {
    this.remove = data;
  }

  @action setDoneIssues(data) {
    this.doneIssues = data;
  }

  @action setTodoIssues(data) {
    this.todoIssues = data;
  }

  @action setRemoveIssues(data) {
    this.removeIssues = data;
  }

  @action setDonePagination(data) {
    this.donePagination = data;
  }

  @action setTodoPagination(data) {
    this.todoPagination = data;
  }

  @action setRemovePagination(data) {
    this.removePagination = data;
  }

  @action setDoneFilter(data) {
    this.doneFilter = data;
  }

  @action setTodoFilter(data) {
    this.todoFilter = data;
  }

  @action setRemoveFilter(data) {
    this.removeFilter = data;
  }

  @action setDoneOrder(data) {
    this.doneOrder = data;
  }

  @action setTodoOrder(data) {
    this.todoOrder = data;
  }

  @action setRemoveOrder(data) {
    this.removeOrder = data;
  }

  @action setChartData(data) {
    this.chartData = data;
  }

  @computed get getCurrentSprintStatus() {
    const STATUS_TIP = {
      closed: {
        status: '已关闭',
        action: '结束',
      },
      started: {
        status: '进行中',
        action: '开启',
      },
    };
    if (!this.currentSprint.statusCode) {
      return ({
        status: '',
        action: '',
      });
    }
    return STATUS_TIP[this.currentSprint.statusCode];
  }
}
const reportStore = new ReportStore();
export default reportStore;
