import { InstanceNode } from "../../types";

export class InstanceDemoData {
  static getInstancesData(): InstanceNode[] {
    return [
      // Local instance (current/selected)
      {
        id: "local-instance",
        label: "★ Local (/home/stachu/code/dark)",
        type: "current",
        contextValue: "current-instance",
        instanceData: {
          path: "/home/stachu/code/dark",
          status: "connected",
        },
        children: []
      },

      // Instance2
      {
        id: "instance2",
        label: "Instance2",
        type: "remote",
        contextValue: "remote-instance",
        instanceData: {
          url: "http://dark-packages.dlio.localhost:11003",
          dbName: "data-instance2.db",
          status: "connected"
        },
        children: []
      }
    ];
  }
}